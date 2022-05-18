//! Tests for lowering
use std::sync::Arc;

use if_chain::if_chain;
use toc_ast_db::{
    db::{AstDatabaseExt, SourceParser},
    SourceGraph,
};
use toc_hir::{body, expr, item, library::LoweredLibrary, stmt, ty};
use toc_hir_lowering::LoweringDb;
use toc_reporting::CompileResult;
use toc_salsa::salsa;
use toc_span::FileId;
use toc_vfs_db::db::VfsDatabaseExt;

#[salsa::database(
    InternedTypeStorage,
    toc_vfs_db::db::FileSystemStorage,
    toc_ast_db::db::SourceParserStorage
)]
#[derive(Default)]
struct TestHirDb {
    storage: salsa::Storage<Self>,
    vfs: toc_vfs::Vfs,
}

impl salsa::Database for TestHirDb {}

toc_vfs::impl_has_vfs!(TestHirDb, vfs);

/// Salsa-backed type interner
#[salsa::query_group(InternedTypeStorage)]
trait InternedType {
    #[salsa::interned]
    fn intern_type(&self, ty: Arc<ty::Type>) -> salsa::InternId;
}

struct LowerResult {
    root_file: FileId,
    hir_result: CompileResult<LoweredLibrary>,
}

fn assert_lower(src: &str) -> LowerResult {
    let mut db = TestHirDb::default();
    let fixture = toc_vfs::generate_vfs(&mut db, src);
    db.insert_fixture(fixture);

    let root_file = db.vfs.intern_path("src/main.t".into());
    let mut source_graph = SourceGraph::new();
    source_graph.add_root(root_file);
    db.set_source_graph(Arc::new(source_graph));
    db.invalidate_source_graph(&toc_vfs::DummyFileLoader);

    let lowered = db.lower_library(root_file);

    let mut s = toc_hir_pretty::tree::pretty_print_tree(lowered.result());
    for err in lowered.messages().iter() {
        s.push_str(&format!("{err}\n"));
    }

    insta::assert_snapshot!(insta::internals::AutoName, s, src);

    LowerResult {
        root_file,
        hir_result: lowered,
    }
}

fn lower_literal_value(expr: &str) -> expr::Literal {
    let lower_result = assert_lower(&format!("var _ : int _ := {expr}"));
    let LowerResult {
        root_file,
        hir_result,
    } = &lower_result;

    let library = hir_result.result();
    let root_item = library.item(library.root_items[root_file]);

    if_chain! {
        if let item::ItemKind::Module(item::Module { body, .. }) = &root_item.kind;
        let body = library.body(*body);
        if let body::BodyKind::Stmts(stmts, ..) = &body.kind;
        if let Some(second_stmt) = stmts.get(1);
        if let stmt::StmtKind::Assign(stmt::Assign { rhs, .. }) = &body.stmt(*second_stmt).kind;
        if let expr::ExprKind::Literal(value) = &body.expr(*rhs).kind;
        then {
            value.clone()
        } else {
            unreachable!();
        }
    }
}

#[test]
fn missing_file_root() {
    assert_lower("%%- removed src/main.t");
}

#[test]
fn item_gathering() {
    // Check that item groups are also included in item gathering
    let res = assert_lower(
        "
    var a, b, c := 0
    const d := 1
",
    );

    let LowerResult {
        root_file,
        hir_result,
    } = res;
    let library = hir_result.result();
    let root_item = library.item(library.root_items[&root_file]);

    if_chain! {
        if let item::ItemKind::Module(item::Module { declares, .. }) = &root_item.kind;
        then {
            assert_eq!(declares.len(), 4);
        } else {
            unreachable!();
        }
    }
}

#[test]
fn lower_var_def() {
    // bare var def
    assert_lower("var a := b");
    // with type spec
    assert_lower("var a : int");
    // just var
    assert_lower("var");
    // check that no def-use cycles are created
    assert_lower("var a := a");
    // just defs
    assert_lower("var a");
    // multiple var defs
    assert_lower("var a, b, c := 0");
}

#[test]
fn lower_simple_assignment() {
    assert_lower("var a, b : int a := b");
    // non-reference lhs
    assert_lower("1 := 2");
    // no rhs
    assert_lower("1 := ");
}

#[test]
fn lower_compound_add_assignment() {
    assert_lower("var a, b : int a += b");
}

#[test]
fn lower_scoping_inner_use_outer_use() {
    // inner & outer uses of `a` and `b` should use the same DefId due to import boundary hoisting
    assert_lower("begin a := b end a := b");
}

#[test]
fn lower_scoping_outer_use_inner_use() {
    // inner & outer uses of `a` and `b` should use the same DefId due to import boundary hoisting
    assert_lower("q := j begin q := k end");
}

#[test]
fn lower_scoping_redeclare_over_def() {
    // Each of these inner defs should be a redcl error
    assert_lower(
        r#"
    var k : int
    begin
        const k := 1
        const k := 2
        const k := 3
    end
    begin
        const k := 1.0
    end
    k := k"#,
    );
}

#[test]
fn lower_scoping_redeclare_use_undef() {
    // should yell about this
    assert_lower(r#"a := a"#);
}

#[test]
fn lower_scoping_redeclare_over_undef() {
    // no qualms about the definition
    assert_lower(r#"a := a const a := 1"#);
}

#[test]
fn lower_int_literal() {
    assert_lower("const a := 01234560");
    // Overflow
    assert_lower("const a := 99999999999999999999");
}

#[test]
fn lower_int_radix_literal() {
    assert_lower("const a := 16#EABC");

    let failing_tests = vec![
        // Overflow
        "10#99999999999999999999",
        // No tail digits
        "30#",
        "30#\n",
        // Out of range (> 36)
        "37#asda",
        // Out of range (= 0)
        "0#0000",
        // Out of range (= 1)
        "1#0000",
        // Out of range (= overflow)
        "18446744073709551616#0000",
        // Invalid digit
        "10#999a9a9",
    ];

    for (num, text) in failing_tests.into_iter().enumerate() {
        eprintln!("On failing test #{}", num + 1);
        let actual_value = lower_literal_value(text);

        // All error literal should produce 0
        if let expr::Literal::Integer(actual_value) = actual_value {
            assert_eq!(actual_value, 0);
        } else {
            unreachable!()
        }
    }
}

#[test]
fn lower_real_literal() {
    let tests = vec![
        // Leading dot
        (".12345", 0.12345),
        // Varying tails
        ("1.", 1.0),
        ("100.00", 100.00),
        ("100.00e10", 100.00e10),
        ("100.00e100", 100.00e100),
        // Invalid format
        ("1e+", 0.0),
        ("1e-", 0.0),
        ("1e", 0.0),
        ("1.0e", 0.0),
        // Too big
        ("1e600", 0.0),
        ("1.0e600", 0.0),
        // Too small (should not produce an error)
        ("1e-999999999", 1e-999999999),
        ("1.0e-999999999", 1.0e-999999999),
    ];

    for (num, (text, value)) in tests.into_iter().enumerate() {
        eprintln!("On test #{}", num + 1);
        let actual_value = lower_literal_value(text);

        if let expr::Literal::Real(actual_value) = actual_value {
            approx::assert_relative_eq!(actual_value, value);
        } else {
            unreachable!()
        }
    }
}

#[test]
fn lower_complex_real_literal() {
    // The following values are from strtod_test.toml
    // See https://github.com/ahrvoje/numerics/blob/master/strtod/strtod_tests.toml

    let tests = vec![
        // Use hex consts for compact representation
        ("C21", 0x000fffffffffffff_u64, "2.225073858507201136057409796709131975934819546351645648023426109724822222021076945516529523908135087914149158913039621106870086438694594645527657207407820621743379988141063267329253552286881372149012981122451451889849057222307285255133155755015914397476397983411801999323962548289017107081850690630666655994938275772572015763062690663332647565300009245888316433037779791869612049497390377829704905051080609940730262937128958950003583799967207254304360284078895771796150945516748243471030702609144621572289880258182545180325707018860872113128079512233426288368622321503775666622503982534335974568884423900265498198385487948292206894721689831099698365846814022854243330660339850886445804001034933970427567186443383770486037861622771738545623065874679014086723327636718749999999999999999999999999999999999999e-308", ),
        ("C22", 0x0010000000000000_u64, "2.22507385850720113605740979670913197593481954635164564802342610972482222202107694551652952390813508791414915891303962110687008643869459464552765720740782062174337998814106326732925355228688137214901298112245145188984905722230728525513315575501591439747639798341180199932396254828901710708185069063066665599493827577257201576306269066333264756530000924588831643303777979186961204949739037782970490505108060994073026293712895895000358379996720725430436028407889577179615094551674824347103070260914462157228988025818254518032570701886087211312807951223342628836862232150377566662250398253433597456888442390026549819838548794829220689472168983109969836584681402285424333066033985088644580400103493397042756718644338377048603786162277173854562306587467901408672332763671875e-308", ),
        ("C23", 0x0010000000000000_u64, "0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000222507385850720138309023271733240406421921598046233183055332741688720443481391819585428315901251102056406733973103581100515243416155346010885601238537771882113077799353200233047961014744258363607192156504694250373420837525080665061665815894872049117996859163964850063590877011830487479978088775374994945158045160505091539985658247081864511353793580499211598108576605199243335211435239014879569960959128889160299264151106346631339366347758651302937176204732563178148566435087212282863764204484681140761391147706280168985324411002416144742161856716615054015428508471675290190316132277889672970737312333408698898317506783884692609277397797285865965494109136909540613646756870239867831529068098461721092462539672851562500000000000000001", ),
        ("C25", 0x7fefffffffffffff_u64, "179769313486231580793728971405303415079934132710037826936173778980444968292764750946649017977587207096330286416692887910946555547851940402630657488671505820681908902000708383676273854845817711531764475730270069855571366959622842914819860834936475292719074168444365510704342711559699508093042880177904174497791.9999999999999999999999999999999999999999999999999999999999999999999999", ),
        ("C29", 0x0000000000000000_u64, "2.47032822920623272e-324", ),
        ("C37", 0x0000000008000000_u64, "6.631236871469758276785396630275967243399099947355303144249971758736286630139265439618068200788048744105960420552601852889715006376325666595539603330361800519107591783233358492337208057849499360899425128640718856616503093444922854759159988160304439909868291973931426625698663157749836252274523485312442358651207051292453083278116143932569727918709786004497872322193856150225415211997283078496319412124640111777216148110752815101775295719811974338451936095907419622417538473679495148632480391435931767981122396703443803335529756003353209830071832230689201383015598792184172909927924176339315507402234836120730914783168400715462440053817592702766213559042115986763819482654128770595766806872783349146967171293949598850675682115696218943412532098591327667236328125E-316", ),
        ("C38", 0x0000000000010000_u64, "3.237883913302901289588352412501532174863037669423108059901297049552301970670676565786835742587799557860615776559838283435514391084153169252689190564396459577394618038928365305143463955100356696665629202017331344031730044369360205258345803431471660032699580731300954848363975548690010751530018881758184174569652173110473696022749934638425380623369774736560008997404060967498028389191878963968575439222206416981462690113342524002724385941651051293552601421155333430225237291523843322331326138431477823591142408800030775170625915670728657003151953664260769822494937951845801530895238439819708403389937873241463484205608000027270531106827387907791444918534771598750162812548862768493201518991668028251730299953143924168545708663913273994694463908672332763671875E-319", ),
        ("C39", 0x0000800000000100_u64, "6.953355807847677105972805215521891690222119817145950754416205607980030131549636688806115726399441880065386399864028691275539539414652831584795668560082999889551357784961446896042113198284213107935110217162654939802416034676213829409720583759540476786936413816541621287843248433202369209916612249676005573022703244799714622116542188837770376022371172079559125853382801396219552418839469770514904192657627060319372847562301074140442660237844114174497210955449896389180395827191602886654488182452409583981389442783377001505462015745017848754574668342161759496661766020028752888783387074850773192997102997936619876226688096314989645766000479009083731736585750335262099860150896718774401964796827166283225641992040747894382698751809812609536720628966577351093292236328125E-310", ),
        ("C40", 0x0000000000010800_u64, "3.339068557571188581835713701280943911923401916998521771655656997328440314559615318168849149074662609099998113009465566426808170378434065722991659642619467706034884424989741080790766778456332168200464651593995817371782125010668346652995912233993254584461125868481633343674905074271064409763090708017856584019776878812425312008812326260363035474811532236853359905334625575404216060622858633280744301892470300555678734689978476870369853549413277156622170245846166991655321535529623870646888786637528995592800436177901746286272273374471701452991433047257863864601424252024791567368195056077320885329384322332391564645264143400798619665040608077549162173963649264049738362290606875883456826586710961041737908872035803481241600376705491726170293986797332763671875E-319", ),
        ("C64", 0x0000000000000000_u64, "2.4703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772285886546332835517796989819938739800539093906315035659515570226392290858392449105184435931802849936536152500319370457678249219365623669863658480757001585769269903706311928279558551332927834338409351978015531246597263579574622766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968951340535537458516661134223766678604162159680461914467291840300530057530849048765391711386591646239524912623653881879636239373280423891018672348497668235089863388587925628302755995657524455507255189313690836254779186948667994968324049705821028513185451396213837722826145437693412532098591327667236328124999e-324", ),
        ("C65", 0x0000000000000000_u64, "2.4703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772285886546332835517796989819938739800539093906315035659515570226392290858392449105184435931802849936536152500319370457678249219365623669863658480757001585769269903706311928279558551332927834338409351978015531246597263579574622766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968951340535537458516661134223766678604162159680461914467291840300530057530849048765391711386591646239524912623653881879636239373280423891018672348497668235089863388587925628302755995657524455507255189313690836254779186948667994968324049705821028513185451396213837722826145437693412532098591327667236328125e-324", ),
        ("C66", 0x0000000000000001_u64, "2.4703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772285886546332835517796989819938739800539093906315035659515570226392290858392449105184435931802849936536152500319370457678249219365623669863658480757001585769269903706311928279558551332927834338409351978015531246597263579574622766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968951340535537458516661134223766678604162159680461914467291840300530057530849048765391711386591646239524912623653881879636239373280423891018672348497668235089863388587925628302755995657524455507255189313690836254779186948667994968324049705821028513185451396213837722826145437693412532098591327667236328125001e-324", ),
        ("C67", 0x0000000000000001_u64, "7.4109846876186981626485318930233205854758970392148714663837852375101326090531312779794975454245398856969484704316857659638998506553390969459816219401617281718945106978546710679176872575177347315553307795408549809608457500958111373034747658096871009590975442271004757307809711118935784838675653998783503015228055934046593739791790738723868299395818481660169122019456499931289798411362062484498678713572180352209017023903285791732520220528974020802906854021606612375549983402671300035812486479041385743401875520901590172592547146296175134159774938718574737870961645638908718119841271673056017045493004705269590165763776884908267986972573366521765567941072508764337560846003984904972149117463085539556354188641513168478436313080237596295773983001708984374999e-324", ),
        ("C68", 0x0000000000000002_u64, "7.4109846876186981626485318930233205854758970392148714663837852375101326090531312779794975454245398856969484704316857659638998506553390969459816219401617281718945106978546710679176872575177347315553307795408549809608457500958111373034747658096871009590975442271004757307809711118935784838675653998783503015228055934046593739791790738723868299395818481660169122019456499931289798411362062484498678713572180352209017023903285791732520220528974020802906854021606612375549983402671300035812486479041385743401875520901590172592547146296175134159774938718574737870961645638908718119841271673056017045493004705269590165763776884908267986972573366521765567941072508764337560846003984904972149117463085539556354188641513168478436313080237596295773983001708984375e-324", ),
        ("C69", 0x0000000000000002_u64, "7.4109846876186981626485318930233205854758970392148714663837852375101326090531312779794975454245398856969484704316857659638998506553390969459816219401617281718945106978546710679176872575177347315553307795408549809608457500958111373034747658096871009590975442271004757307809711118935784838675653998783503015228055934046593739791790738723868299395818481660169122019456499931289798411362062484498678713572180352209017023903285791732520220528974020802906854021606612375549983402671300035812486479041385743401875520901590172592547146296175134159774938718574737870961645638908718119841271673056017045493004705269590165763776884908267986972573366521765567941072508764337560846003984904972149117463085539556354188641513168478436313080237596295773983001708984375001e-324", ),
        ("C76", 0x0006c9a143590c14_u64, "94393431193180696942841837085033647913224148539854e-358", ),
        ("C79", 0x0007802665fd9600_u64, "104308485241983990666713401708072175773165034278685682646111762292409330928739751702404658197872319129036519947435319418387839758990478549477777586673075945844895981012024387992135617064532141489278815239849108105951619997829153633535314849999674266169258928940692239684771590065027025835804863585454872499320500023126142553932654370362024104462255244034053203998964360882487378334860197725139151265590832887433736189468858614521708567646743455601905935595381852723723645799866672558576993978025033590728687206296379801363024094048327273913079612469982585674824156000783167963081616214710691759864332339239688734656548790656486646106983450809073750535624894296242072010195710276073042036425579852459556183541199012652571123898996574563824424330960027873516082763671875e-1075", ),
    ];

    for (name, hex_value, text) in tests.into_iter() {
        eprintln!("On test {name}");
        let actual_value = lower_literal_value(text);
        let value = f64::from_ne_bytes(hex_value.to_ne_bytes());

        if let expr::Literal::Real(actual_value) = actual_value {
            approx::assert_relative_eq!(actual_value, value);
        } else {
            unreachable!()
        }
    }
}

#[test]
fn lower_string_literal() {
    assert_lower(r#"const a := "abcd💖""#);

    // Should handle strings without an ending delimiter
    assert_lower(r#"const a := "abcd "#);
    // ... or mismatched delimiter
    assert_lower(r#"const a := "abcd'"#);
}

#[test]
fn lower_char_literal() {
    assert_lower(r#"const a := 'abcd💖'"#);

    // Should handle character strings without an ending delimiter
    assert_lower(r#"const a := 'abcd "#);
    // ... or mismatched delimiter
    assert_lower(r#"const a := 'abcd""#);
    // ... or that are completely empty
    assert_lower(r#"const a := ''"#);
    // ... or that are completely empty without an ending delimiter
    assert_lower(r#"const a := '"#);
}

#[test]
fn lower_char_seq_escapes() {
    // All escapes:
    let escapes = vec![
        // Backslash escapes
        (r#""\\""#, "\\"),
        (r#""\'""#, "\'"),
        (r#""\"""#, "\""),
        (r#""\b""#, "\x08"),
        (r#""\d""#, "\x7F"),
        (r#""\e""#, "\x1B"),
        (r#""\f""#, "\x0C"),
        (r#""\r""#, "\r"),
        (r#""\n""#, "\n"),
        (r#""\t""#, "\t"),
        (r#""\^""#, "^"),
        (r#""\B""#, "\x08"),
        (r#""\D""#, "\x7F"),
        (r#""\E""#, "\x1B"),
        (r#""\F""#, "\x0C"),
        (r#""\T""#, "\t"),
        // Octal escapes
        (r#""\0o""#, "\0o"),
        (r#""\43O""#, "#O"),
        (r#""\101""#, "A"),
        (r#""\377""#, "\u{00FF}"), // Have to use unicode characters
        (r#""\1011""#, "A1"),
        // Hex escapes (non-hex digits and extra hex digits are ignored)
        (r#""\x0o""#, "\0o"),
        (r#""\x00""#, "\0"),
        (r#""\x00Ak""#, "\0Ak"),
        (r#""\x20""#, " "),
        (r#""\x20Ar""#, " Ar"),
        (r#""\xfe""#, "\u{00FE}"),
        // Unicode escapes (non-hex digits and extra digits are ignored)
        (r#""\u8o""#, "\x08o"),
        (r#""\uA7k""#, "§k"),
        (r#""\u394o""#, "Δo"),
        (r#""\u2764r""#, "❤r"),
        (r#""\u1f029t""#, "🀩t"),
        (r#""\u10f029s""#, "\u{10F029}s"),
        (r#""\u10F029i""#, "\u{10F029}i"),
        (r#""\U8O""#, "\x08O"),
        (r#""\Ua7l""#, "§l"),
        (r#""\U394w""#, "Δw"),
        (r#""\U2764X""#, "❤X"),
        (r#""\U1F029z""#, "🀩z"),
        (r#""\U10F029Y""#, "\u{10F029}Y"),
        (r#""\U10F029jY""#, "\u{10F029}jY"),
        // Caret escapes
        (r#""^J""#, "\n"),
        (r#""^M""#, "\r"),
        (r#""^?""#, "\x7F"),
        // Unterminated literal
        (r#"""#, ""),
        // Invalid Escapes //
        // Without any following
        (r#""\"#, ""),
        (r#""^"#, ""),
        // Caret Escaped literals
        (r#""^""#, r#"""#),
        // Greater than 255
        (r#""\777""#, "\u{FFFD}"),
        // Larger than U+10FFFF
        (r#""\u200000""#, "\u{FFFD}"),
        (r#""\u3ffffff""#, "\u{FFFD}"),
        (r#""\u3fffffff""#, "\u{FFFD}"),
        // Surrogate characters
        (r#""\uD800""#, "\u{FFFD}"),
        (r#""\UDFfF""#, "\u{FFFD}"),
        (r#""\Ud900""#, "\u{FFFD}"),
        (r#""\udab0""#, "\u{FFFD}"),
        // Incorrect start of escape sequence
        (r#""\8""#, "8"),
        (r#""^~""#, "~"),
        (r#""\x""#, "x"),
        (r#""\u""#, "u"),
        (r#""\U""#, "U"),
    ];

    for (text, expected_value) in escapes.into_iter() {
        let stringified_test = format!("({text:?}, {expected_value:?}, ..)");
        assert_eq!(
            lower_literal_value(text),
            expr::Literal::String(expected_value.to_string()),
            "At \"{}\"",
            stringified_test
        );
    }
}

#[test]
fn lower_multiple_invalid_char_seq_escapes() {
    assert_eq!(
        lower_literal_value(r#"'\777\ud800\!'"#),
        expr::Literal::CharSeq("\u{FFFD}\u{FFFD}!".to_string()),
    );
}

#[test]
fn lower_paren_expr() {
    assert_lower("const a := (1)");
    // nested
    assert_lower("const a := (((1)))");
    // empty
    assert_lower("const a := ()");
}

#[test]
fn lower_self_expr() {
    assert_lower("const a := self");
}

#[test]
fn lower_binary_expr() {
    assert_lower("const a := 1 + 1");
    // missing operand, should still be present
    assert_lower("const a := () + ");
    // invalid infix, okay to be missing
    assert_lower("const a := 1 not 1 ");
}

#[test]
fn lower_unary_expr() {
    assert_lower("const a := + a");
    // missing operand, should still be present
    assert_lower("const a := +");
}

#[test]
fn lower_prim_type() {
    let tys = vec![
        "int",
        "int1",
        "int2",
        "int4",
        "nat",
        "nat1",
        "nat2",
        "nat4",
        "real",
        "real4",
        "real8",
        "boolean",
        "addressint",
        "char",
        "string",
    ];

    for ty_text in tys {
        assert_lower(&format!("var _ : {ty_text}"));
    }
}

#[test]
fn lower_prim_char_seq_type() {
    assert_lower("var _ : char(1)");
}

#[test]
fn lower_put_stmt() {
    // single item
    assert_lower("put a");
    // many items
    assert_lower("put skip, a : 1, b : 2 : 3, c : 4 : 5 : 6");
    // no item
    assert_lower("put");
    // missing exprs
    assert_lower("put 1 : 1 :  : 1");
    assert_lower("put 1 : 1 :  : ");
    assert_lower("put 1 :  :  : ");

    // stream expr
    assert_lower("put : a");
    // no items, but there's a stream expr
    assert_lower("put : 1");
}

#[test]
fn lower_get_stmt() {
    // single item
    assert_lower("get a");
    // many items
    assert_lower("get skip, a, b : 1, c : *");
    // no items
    assert_lower("get");
    // not a reference
    assert_lower("get a*a");

    // stream expr
    assert_lower("get a : a*a");
    // no items, but there's a stream expr
    assert_lower("get : 1");
}

#[test]
fn lower_block_stmt_multiple() {
    assert_lower("begin var _ := 0 _ := 1 _ := 2 _ := 3 end");
}

#[test]
fn lower_multiple_stmts() {
    assert_lower("var _ := 0 _ := 1 _ := 2 _ := 3");
}

#[test]
fn expression_order() {
    assert_lower("var _ := 1 + 2 * 3 + 4");
}

#[test]
fn lower_if_stmt() {
    // if
    assert_lower(
        r#"
    if true then
        var a := 1
    end if
    "#,
    );
    // if else
    assert_lower(
        r#"
    if true then
        var a := 1
    else
        var a := 2
    end if
    "#,
    );
    // if elseif
    assert_lower(
        r#"
    if true then
        var a := 1
    elsif false then
        var a := 2
    end if
    "#,
    );
    // if elseif else
    assert_lower(
        r#"
    if true then
        var a := 1
    elsif false then
        var a := 2
    else
        var a := 3
    end if
    "#,
    );
    // if elseif elseif else
    assert_lower(
        r#"
    if true then
        var a := 1
    elsif false then
        var a := 2
    elsif true then
        var a := 3
    else
        var a := 4
    end if
    "#,
    );
    // dangling
    assert_lower(
        r#"
    elsif true then end if
    elif true then end if
    elseif true then end if
    else end if
    "#,
    );
    // with missing condition
    assert_lower("if then end if");
}

#[test]
fn lower_loop_stmt() {
    assert_lower("loop end loop");
}

#[test]
fn lower_exit_stmt() {
    // in loop body
    assert_lower(r#"loop exit end loop"#);
    // in for-loop body
    assert_lower(r#"for i : 1 .. 10 exit end for"#);
    // outside of a loop body
    assert_lower("exit");
    // with optional condition
    assert_lower("loop exit when true end loop");
}

#[test]
fn lower_for_stmt() {
    // bare loop
    assert_lower(r#"for : 1 .. 10 end for"#);
    // explicit bounds & step
    assert_lower(r#"for : 1 .. 10 by 3 end for"#);
    // implicit bounds
    assert_lower(r#"var wah : int for : wah end for"#);
    // implicit bounds & step
    assert_lower(r#"var wah : int for : wah by 1 end for"#);
    // with counter
    assert_lower(
        r#"
    for woo : 1 .. 10 var k := woo end for
    var woo := 1.0
    "#,
    );
    // decreasing explicit bounds
    assert_lower(r#"for decreasing : 1 .. 10 end for"#);
    // decreasing implicit bounds (error)
    assert_lower(r#"var implied : int for decreasing : implied end for"#);
    // no bounds
    assert_lower(r#"for : end for"#);
    // decreasing no bounds
    assert_lower(r#"for decreasing : end for"#);
}

#[test]
fn lower_case_stmt() {
    // no arms
    assert_lower(r#"case s of end case"#);
    // one default arm
    assert_lower(r#"case s of label: end case"#);
    // one arm
    assert_lower(r#"case s of label 1: end case"#);
    // many arms
    assert_lower(
        r#"
    case s of
    label 1:
        var a := a + 1
    label 2, 3:
        var a := a + 2
    label 4:
        var a := a + 3
    end case
    "#,
    );
    // many arms, default
    assert_lower(
        r#"
    case s of
    label 1:
        var a := a + 1
    label 2, 3:
        var a := a + 2
    label 4:
        var a := a + 3
    label :
        var a := a + 4
    end case
    "#,
    );
    // many arms, default, default arm
    assert_lower(
        r#"
    case s of
    label 1:
        var a := a + 1
    label 2, 3:
        var a := a + 2
    label 4:
        var a := a + 3
    label :
        var a := a + 4
    label :
        var a := a + 5
    end case
    "#,
    );
    // many arms, default, arms
    assert_lower(
        r#"
    case s of
    label 1:
        var a := a + 1
    label 2, 3:
        var a := a + 2
    label 4:
        var a := a + 3
    label :
        var a := a + 4
    label 5:
        var a := a + 5
    label 6:
        var a := a + 6
    end case
    "#,
    );
}

#[test]
fn lower_type_def() {
    // with type
    assert_lower("type alias : int");
    // with forward
    assert_lower("type alias : forward");
    // missing defined
    assert_lower("type alias : ");
    // missing name
    assert_lower("type : forward");
}

#[test]
fn lower_type_alias() {
    assert_lower(
        "
    type a : int
    type use_it : a
    var _ : a
    ",
    );
    // type aliases also cover general expressions in type position, which are invalid
    assert_lower(
        "
    type a : 1 + 1
    ",
    );
}

#[test]
fn lower_type_path() {
    // Chained type paths
    assert_lower(
        "
    type a : int
    type chain: a.b.c.d.e
    ",
    );

    // Stops at the first non-name
    assert_lower("type soup: a.b().c");
}

#[test]
fn lower_type_def_forward() {
    // Normal
    assert_lower(
        "
    type a : forward
    type use_it : a
    type a : int
    ",
    );
    // Duplicate forward declare
    assert_lower(
        "
    type a : forward
    type a : forward
    type use_it : a
    type a : int
    ",
    );
    // Must be resolved in the same scope
    assert_lower(
        "
    type a : forward
    begin
        type a : int
    end
    type use_it : a
    ",
    );
    // Different declarations should leave forwards unresolved
    assert_lower(
        "
    type a : forward
    type use_it : a % should not be resolved to latter a
    var a : int
    type a : char
    ",
    );
    // TODO: Add test for different forward kinds
}

#[test]
fn lower_binding_def() {
    // Normal
    assert_lower(
        "
    begin
    var a : int
    bind b to a
    end",
    );
    // Multiple
    assert_lower(
        "
    begin
    var a : int
    bind b to a, var c to b, var register d to c
    end",
    );
    // Binding to non-ref expr
    // Accept here, but rejected in typeck
    assert_lower("begin bind me to true or false end");
    // Lacking portions
    assert_lower(
        "
    begin
    bind to 1, k to
    end",
    );
    // In top-level scope
    assert_lower(
        "
    var nothing := 0
    bind me to nothing",
    );
}

#[test]
fn lower_procedure_def() {
    // Without param list
    assert_lower(
        "
    procedure no_params
    end no_params",
    );

    // None specified
    assert_lower(
        "
    procedure empty_params()
    end empty_params
    ",
    );

    // With params
    assert_lower(
        "
    procedure some_params(a, b : int)
        % should be visible
        var me := a + b
    end some_params
    ",
    );

    // Different passings
    assert_lower(
        "
    procedure pass_me(
        by_value : int,
        var by_ref : int,
        register by_val_to_reg : int,
        var register by_ref_to_reg : int
    )
    end pass_me
    ",
    );

    // Device spec
    // FIXME: Embed in monitor module to get rid of the errors
    assert_lower(
        "
    procedure a : 4 + 4 end a
    procedure b() : 6 + 8 end b
    procedure c(k : int) : 9 + 2 end c
    ",
    );

    // Redecl over params
    assert_lower(
        "
    procedure pars(a, a : int, a : int1)
        var a : int2
    end pars",
    );
}

#[test]
fn lower_function_def() {
    // Without param list
    assert_lower(
        "
    function no_params : int
    end no_params",
    );

    // None specified
    assert_lower(
        "
    function empty_params() : int
    end empty_params
    ",
    );

    // With params
    assert_lower(
        "
    function some_params(a, b : int) : int
        % should be visible
        var me := a + b
    end some_params
    ",
    );

    // Different passings
    assert_lower(
        "
    function pass_me(
        by_value : int,
        var by_ref : int,
        register by_val_to_reg : int,
        var register by_ref_to_reg : int
    ) : int
    end pass_me
    ",
    );

    // Named result
    assert_lower(
        "
    function a shambles : int end a
    function b() quoi : int end b
    function c(k : int) weh : int end c
    ",
    );

    // Redecl over params
    assert_lower(
        "
    function pars(a, a : int, a : int1) : int
        var a : int2
    end pars",
    );

    // Redecl over named result
    assert_lower(
        "
    function pars() res : int
        var res : int1
    end pars",
    );

    // Not using named result in `post` condition
    assert_lower(
        "
    function pars() res : int
        var use := res
    end pars",
    );
}

#[test]
fn lower_process_def() {
    // Without param list
    assert_lower(
        "
    process no_params
    end no_params",
    );

    // None specified
    assert_lower(
        "
    process empty_params()
    end empty_params
    ",
    );

    // With params
    assert_lower(
        "
    process some_params(a, b : int)
        % should be visible
        var me := a + b
    end some_params
    ",
    );

    // Different passings
    assert_lower(
        "
    process pass_me(
        by_value : int,
        var by_ref : int,
        register by_val_to_reg : int,
        var register by_ref_to_reg : int
    )
    end pass_me
    ",
    );

    // Stack size
    assert_lower(
        "
    process a : 4 + 4 end a
    process b() : 6 + 8 end b
    process c(k : int) : 9 + 2 end c
    ",
    );

    // Redecl over params
    assert_lower(
        "
    process pars(a, a : int, a : int1)
        var a : int2
    end pars",
    );
}

#[test]
fn lower_subprogram_shadow_external() {
    // Formal names are always allowed to shadow external names (even builtin ones)
    // Names in subprograms are only allowed to shadow non-pervasive names
    assert_lower(
        "
    var pervasive pv_formal, pv_inner : int
    var norm_formal, norm_inner : int

    procedure shade(pv_formal : real, norm_formal : real)
        var _ : real
        var norm_inner : real
        var pv_inner : real % only this is rejected

        _ := pv_formal
        _ := pv_inner
        _ := norm_formal
        _ := norm_inner
    end shade",
    );
}

#[test]
fn lower_formals_use_name() {
    assert_lower("type i : int proc u (j : i) end u");
}

#[test]
fn lower_formals_cheat_attr() {
    assert_lower("proc u (j : cheat int) end u");
}

#[test]
fn lower_formals_intersperse_missing() {
    // Only 1 arg, with trailing comma
    assert_lower("procedure args(sa, : int) end args");
    // Interspersed 1 arg
    assert_lower("procedure args(sa, , ba: int) end args");
    // Trailing 3 missing args
    assert_lower("procedure args(a, , , , : int) end args");
    // 5 args, with 3 interspersed
    assert_lower("procedure args(a, , , , b, : int) end args");

    // Only commas (doesn't work right now due to poor recovery)
    // FIXME: Uncomment when name list recovery gets better

    // Missing, 1 arg
    // assert_lower("procedure args(, : int) end args");
    // Missing 4 args
    // assert_lower("procedure args(, , , , : int) end args");
}

#[test]
fn lower_procedure_tys() {
    // Taken from the old compiler
    assert_lower("type _ : procedure nps");
    assert_lower("type _ : procedure np   ()");
    assert_lower("type _ : procedure p1   (a : int)");
    assert_lower("type _ : procedure p2   (a : int, b : string)");
    assert_lower("type _ : procedure pisp (a : int, b : string, c : procedure _ ())");

    // Identifier is optional
    assert_lower("type _ : procedure");
}

#[test]
fn lower_function_tys() {
    // Taken from the old compiler
    assert_lower("type _ : function np   () : real");
    assert_lower("type _ : function p1   (a : int) : string");
    assert_lower("type _ : function p2   (a : int, b : string) : addressint");
    assert_lower("type _ : function pisf (a : int, b : string, c : function _ () : int) : boolean");
    // Nesting
    assert_lower("type _ : function _ (function a (function a : int ) : int, proc b (proc a (proc a( proc a))), proc c) : int");

    // Identifier is optional
    assert_lower("type _ : function () : int");

    // Function type expects `function () : int` instead of `function : int` since it would be uninhabitable
    // (bare reference would always call the function)
    //
    // Treated as a warning, since it's technically valid syntax
    // Emitted from AST validation
    assert_lower("type _ : function amphy : int");
}

#[test]
fn lower_call_expr() {
    // Bare
    // (treated as name expr, since we need the type to disambiguate between the actual cases)
    // TODO: Handle parameterless calls
    assert_lower("proc call end call var _ := call");
    // No params
    assert_lower("proc call end call var _ := call()");
    // Some args
    assert_lower("proc call end call var _ := call(1,2,3)");

    // Missing trailing
    assert_lower("proc call end call var _ := call(1,,)");
    // Missing
    assert_lower("proc call end call var _ := call(,,)");
}

#[test]
fn lower_all_expr() {
    // In expected position
    assert_lower("proc call end call var _ := call(all)");
    // Not in expected position
    assert_lower("var _ := all");
}

#[test]
fn lower_range_expr() {
    // In expected position
    // AtEnd
    assert_lower("proc call end call var _ := call(*)");
    // FromStart..FromStart
    assert_lower("proc call end call var _ := call(1..2)");
    // FromStart..AtEnd
    assert_lower("proc call end call var _ := call(1..*)");
    // FromStart..FromEnd
    assert_lower("proc call end call var _ := call(1..*-1)");
    // AtEnd..FromStart
    assert_lower("proc call end call var _ := call(*..1)");
    // FromEnd..FromStart
    assert_lower("proc call end call var _ := call(*-1..1)");

    // Not in expected position
    assert_lower("var _ := *..*");
}

#[test]
fn lower_call_stmt() {
    // Just name
    assert_lower("proc call end call call");
    // `Call` is inlined
    assert_lower("proc call end call call()");

    // Arbitrary exprs are treated as calls
    assert_lower("1");
}

#[test]
fn lower_return_stmt() {
    assert_lower("return");
}

#[test]
fn lower_result_stmt() {
    assert_lower(r#"fcn _ : string result "sus" end _"#);
}

#[test]
fn lower_module_def() {
    assert_lower("module a var c := 2 end a");

    assert_lower("module var c := 2 end a");

    // Module name should be visible inside of the module itself
    assert_lower(
        "
    module pervasive a
        module b
            module c
                a b c
            end c
        end b
    end a
    ",
    );

    // Unqualified (pervasive) exports should be visible in the sibling scopes
    assert_lower(
        "
        module z
            export ~.a, ~.b
            var a, b : int
        end z
        module y
            export ~.*all
            var c, d : int
        end y

        % non pervasive
        var i : int
        i := a
        i := b

        % pervasive
        module x
            var i : int
            i := c
            i := d
        end x
        ",
    );

    // Report when unqualified exports are occluding others
    assert_lower(
        "
        var u, w, U : int
        module a
            export ~.all
            var u, w, U : int
        end a
        ",
    );
}

#[test]
fn lower_module_exports() {
    assert_lower(
        "
    module tree
        export a, var b, unqualified c, pervasive unqualified d, opaque e

        var a, b, c, d : int
        type e : int
    end tree",
    );

    // pervasive only applicable to unqualified
    assert_lower(
        "
    module z
        export *a
        var a : int
    end z",
    );

    // opaque only applicable to types
    assert_lower(
        "
        module z
            export opaque a, opaque z
            var a : int
        end z",
    );

    // undeclared export
    assert_lower(
        "
        module z
            export a
        end z",
    );

    // only export from inside module
    assert_lower(
        "
        var *a : int
        module z
            export a
        end z",
    );

    assert_lower(
        "
    module z
        export var *~. all
        var heres, tree : int
    end z",
    );

    // If an all is present, the rest of the exports are ignored
    assert_lower(
        "
    module z
        export var *~. all, heres
        var heres, tree : int
    end z",
    );

    // `all` opaque restriction only applies to applicable items
    assert_lower(
        "
    module z
        export var *~. opaque all
        var heres, tree : int
        type again : int
    end z",
    );

    // duplicate export
    assert_lower(
        "
    module z
        export a, a, a, a, a
        var a : int
    end z",
    );

    // unapplicable mutability
    assert_lower(
        "
    module z
        export var a, var b
        type a : int
        const b := 8080
    end z",
    );

    // duplicate all
    assert_lower(
        "
    module z
        export all, all, all
    end z",
    );
}

#[test]
fn lower_field_expr() {
    assert_lower(
        "
    module a end a
    a.b
    ",
    );

    // Missing field name
    assert_lower(
        "
    module a end a
    a.
    ",
    );
}

#[test]
fn lower_set_ty() {
    assert_lower("type _ : set of boolean");
    // should still be present even with missing type
    assert_lower("type _ : set of ");
    // in storage position
    assert_lower("var _ : set of boolean");
}

#[test]
fn lower_deref_expr() {
    assert_lower("var a : int var _ := ^a ");
    assert_lower("var a : int var _ := ^(a) ");
    assert_lower("var a : int var _ := ^ ");
    assert_lower("var a : int var _ := ^^^^a ");
    assert_lower("var a : int ^a()");
}

#[test]
fn lower_pointer_ty() {
    // Both variations
    assert_lower("type _ : pointer to int");
    assert_lower("type _ : ^int");

    // Both variations of unchecked pointer
    assert_lower("type _ : unchecked ^int");
    assert_lower("type _ : unchecked pointer to int");

    // Missing ty
    assert_lower("type _ : pointer to");
    assert_lower("type _ : ^");
    assert_lower("type _ : unchecked ^");
    assert_lower("type _ : unchecked pointer to");

    // Missing `to`
    assert_lower("type _ : pointer int");
    assert_lower("type _ : unchecked pointer int");

    // Just unchecked
    assert_lower("type _ : unchecked int");
}

#[test]
fn lower_enum_type() {
    // one variant
    assert_lower("type _ : enum(a)");
    // multiple variants
    assert_lower("type _ : enum(a, b, c, d)");

    // missing end
    assert_lower("type _ : enum(a, )");
    // missing middle
    assert_lower("type _ : enum(a, , d)");
    // missing start
    assert_lower("type _ : enum(, a)");

    // no variants
    assert_lower("type _ : enum(a)");
}

#[test]
fn lower_constrained_type_sized() {
    // Full range
    assert_lower("var _ : 1 .. 2");
    // Missing bits
    // ???: We could support this syntax in the future
    assert_lower("var _ : .. 2");
    assert_lower("var _ : 1 ..");
    assert_lower("var _ : ..");
}

#[test]
fn lower_constrained_type_unsized() {
    // Valid everything
    assert_lower("var _ : array 1 .. * of int := init(1, 2, 3)");
    // Invalid initializer
    assert_lower("var _ : array 1 .. * of int := 2");
    // Missing initializer
    assert_lower("var _ : array 1 .. * of int");
    // Outside of an array
    assert_lower("var _ : 1 .. *");
    assert_lower("type _ : 1 .. *");
}

#[test]
fn lower_array_types() {
    // One bound
    assert_lower("type _ : array char of int");
    // Multiple bounds
    assert_lower("type _ : array char, boolean, 1..2 of int");
    // No bounds
    assert_lower("type _ : array of int");

    // Maybe dyn
    assert_lower("var _ : array boolean of int");

    // Flexible
    assert_lower("var _ : flexible array 1 .. 0 of int");
}

#[test]
fn lower_array_types_init_sized() {
    // Init-sized
    assert_lower("var _ : array 1 .. * of int := init(1)");
    // Only 1 init-sized range allowed
    assert_lower("var _ : array 1 .. *, 1 .. * of int := init(1)");
    // Init-sized range must be the first
    // ranges before
    assert_lower("var _ : array 1 .. *, char of int := init(1)");
    assert_lower("var _ : array 1 .. *, char,,char of int := init(1)");
    // ranges after
    assert_lower("var _ : array char, 1 .. * of int := init(1)");
    assert_lower("var _ : array char,,char, 1 .. * of int := init(1)");
    // both
    assert_lower("var _ : array char, 1 .. *, char of int := init(1)");
}

#[test]
fn lower_array_types_any_sized() {
    // Same restrictions as init-sized, just in param decl context

    // Any-sized
    assert_lower("type _ : proc uwu(_ : array 1 .. * of int)");
    // Only 1 any-sized range allowed
    assert_lower("type _ : proc uwu(_ : array 1 .. *, 1 .. * of int)");
    // Any-sized range must be the first
    // ranges before
    assert_lower("type _ : proc uwu(_ : array 1 .. *, char of int)");
    assert_lower("type _ : proc uwu(_ : array 1 .. *, char,,char of int)");
    // ranges after
    assert_lower("type _ : proc uwu(_ : array char, 1 .. * of int)");
    assert_lower("type _ : proc uwu(_ : array char,,char, 1 .. * of int)");
    // both
    assert_lower("type _ : proc uwu(_ : array char, 1 .. *, char of int)");
}

#[test]
fn lower_init_expr() {
    // Ok with non-aggregate types (check deferred to typeck)
    assert_lower("var _ : int := init(1, 2, 3, 4)");
    // Type spec required (handled by AST validation)
    assert_lower("var _ := init(1, 2, 3, 4)");
    // Can't be used outside of a ConstVar decl (handled by AST validation)
    assert_lower("var a : int a := init(1, 2, 3, 4)");
}

#[test]
fn unsupported_external_import() {
    assert_lower("import()");
}

#[test]
fn lower_import_stmt() {
    // Only lowering import statements
    assert_lower(
        "
    var a, b, c : int
    module _
        import a, var c, const c
    end _",
    );

    // const with var
    assert_lower(
        "
    var a : int
    module _
        import const var a
    end _",
    );

    // forward (unsupported)
    assert_lower(
        "
    var a : int
    module _
        import forward a
    end _",
    );
}
