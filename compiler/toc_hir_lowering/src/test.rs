//! Tests for lowering
use if_chain::if_chain;
use toc_hir::{expr, stmt};

use crate::HirLowerResult;

fn stringify_unit(unit: &toc_hir::Unit) -> String {
    let mut s = String::new();

    fn dump_spanned_arena<T>(s: &mut String, name: &str, arena: &toc_hir::SpannedArena<T>)
    where
        T: std::fmt::Debug,
    {
        s.push_str(&format!("{}:\n", name));
        for (id, node) in arena.arena.iter() {
            let span = arena.spans.get(&id).unwrap();
            s.push_str(&format!("{:?} ({:?}): {:?}\n", id, span, node));
        }
        s.push('\n');
    }

    // Dump node database
    s.push_str("database:\n");
    dump_spanned_arena(&mut s, "stmt", &unit.database.stmt_nodes);
    dump_spanned_arena(&mut s, "expr", &unit.database.expr_nodes);
    dump_spanned_arena(&mut s, "ty", &unit.database.type_nodes);

    // List
    s.push_str("root stmts:\n");
    s.push_str(&format!("{:?}\n", unit.stmts));
    // Show
    s.push_str("symtab:\n");
    let mut defs = unit.symbol_table.iter_defs().collect::<Vec<_>>();
    defs.sort_by_key(|(id, _, _)| *id);
    for (id, span, sym) in defs {
        s.push_str(&format!("{:?}: ({:?}, {:?})\n", id, span, sym));
    }

    let mut uses = unit.symbol_table.iter_uses().collect::<Vec<_>>();
    uses.sort_by_key(|(id, _)| *id);
    for (id, span) in uses {
        s.push_str(&format!("{:?}: {:?}\n", id, span));
    }

    s
}

fn assert_lower(src: &str) -> HirLowerResult {
    let parse = toc_parser::parse(src);
    let lowered = crate::lower_ast(parse.syntax());

    let mut s = stringify_unit(&lowered.unit);
    for err in &lowered.messages {
        s.push_str(&format!("{}\n", err));
    }

    insta::assert_snapshot!(insta::internals::AutoName, s, src);

    lowered
}

fn literal_value(lowered: &HirLowerResult) -> &expr::Literal {
    if_chain! {
        if let stmt::Stmt::Assign(stmt::Assign { rhs, .. }) = &lowered.unit.database[lowered.unit.stmts[0]];
        if let expr::Expr::Literal(value) = &lowered.unit.database[*rhs];
        then {
            value
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
}

#[test]
fn lower_simple_assignment() {
    assert_lower("a := b");
}

#[test]
fn lower_compound_add_assignment() {
    assert_lower("a += b");
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
fn lower_int_literal() {
    assert_lower("a := 01234560");
    // Overflow
    assert_lower("a := 99999999999999999999");
}

#[test]
fn lower_int_radix_literal() {
    assert_lower("a := 16#EABC");

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
        let lowered = assert_lower(&format!("a := {}", text));
        let actual_value = literal_value(&lowered);

        // All error literal should produce 0
        if let expr::Literal::Integer(actual_value) = actual_value {
            assert_eq!(*actual_value, 0);
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
        let lowered = assert_lower(&format!("a := {}", text));
        let actual_value = literal_value(&lowered);

        if let expr::Literal::Real(actual_value) = actual_value {
            approx::assert_relative_eq!(*actual_value, value);
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
        // Literals not parsed by rustc
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
        eprintln!("On test {}", name);
        let lowered = assert_lower(&format!("a := {}", text));
        let actual_value = literal_value(&lowered);
        let value = f64::from_ne_bytes(hex_value.to_ne_bytes());

        if let expr::Literal::Real(actual_value) = actual_value {
            approx::assert_relative_eq!(*actual_value, value);
        } else {
            unreachable!()
        }
    }
}

#[test]
fn lower_string_literal() {
    assert_lower(r#"a := "abcd💖""#);

    // Should handle strings without an ending delimiter
    assert_lower(r#"a := "abcd "#);
    // ... or mismatched delimiter
    assert_lower(r#"a := "abcd'"#);
}

#[test]
fn lower_char_literal() {
    assert_lower(r#"a := 'abcd💖'"#);

    // Should handle character strings without an ending delimiter
    assert_lower(r#"a := 'abcd "#);
    // ... or mismatched delimiter
    assert_lower(r#"a := 'abcd""#);
}

#[test]
fn lower_char_seq_escapes() {
    // All escapes:
    let escapes = vec![
        // Backslash escapes
        ("'\\\\'", "\\"),
        ("'\\\''", "\'"),
        ("'\\\"'", "\""),
        ("'\\b'", "\x08"),
        ("'\\d'", "\x7F"),
        ("'\\e'", "\x1B"),
        ("'\\f'", "\x0C"),
        ("'\\r'", "\r"),
        ("'\\n'", "\n"),
        ("'\\t'", "\t"),
        ("'\\^'", "^"),
        ("'\\B'", "\x08"),
        ("'\\D'", "\x7F"),
        ("'\\E'", "\x1B"),
        ("'\\F'", "\x0C"),
        ("'\\T'", "\t"),
        // Octal escapes
        ("'\\0o'", "\0o"),
        ("'\\43O'", "#O"),
        ("'\\101'", "A"),
        ("'\\377'", "\u{00FF}"), // Have to use unicode characters
        ("'\\1011'", "A1"),
        // Hex escapes (non-hex digits and extra hex digits are ignored)
        ("'\\x0o'", "\0o"),
        ("'\\x00'", "\0"),
        ("'\\x00Ak'", "\0Ak"),
        ("'\\x20'", " "),
        ("'\\x20Ar'", " Ar"),
        ("'\\xfe'", "\u{00FE}"),
        // Unicode escapes (non-hex digits and extra digits are ignored)
        ("'\\u8o'", "\x08o"),
        ("'\\uA7k'", "§k"),
        ("'\\u394o'", "Δo"),
        ("'\\u2764r'", "❤r"),
        ("'\\u1f029t'", "🀩t"),
        ("'\\u10f029s'", "\u{10F029}s"),
        ("'\\u10F029i'", "\u{10F029}i"),
        ("'\\U8O'", "\x08O"),
        ("'\\Ua7l'", "§l"),
        ("'\\U394w'", "Δw"),
        ("'\\U2764X'", "❤X"),
        ("'\\U1F029z'", "🀩z"),
        ("'\\U10F029Y'", "\u{10F029}Y"),
        ("'\\U10F029jY'", "\u{10F029}jY"),
        // Caret escapes
        ("'^J'", "\n"),
        ("'^M'", "\r"),
        ("'^?'", "\x7F"),
        // Unterminated literal
        ("'", ""),
        // Invalid Escapes //
        // Without any following
        ("'\\", ""),
        ("'^", ""),
        // Caret Escaped literals
        ("'^'", "'"),
        // Greater than 255
        ("'\\777'", "\u{FFFD}"),
        // Larger than U+10FFFF
        ("'\\u200000'", "\u{FFFD}"),
        ("'\\u3ffffff'", "\u{FFFD}"),
        ("'\\u3fffffff'", "\u{FFFD}"),
        // Surrogate characters
        ("'\\uD800'", "\u{FFFD}"),
        ("'\\UDFfF'", "\u{FFFD}"),
        ("'\\Ud900'", "\u{FFFD}"),
        ("'\\udab0'", "\u{FFFD}"),
        // Incorrect start of escape sequence
        ("'\\8'", "8"),
        ("'^~'", "~"),
        ("'\\x'", "x"),
        ("'\\u'", "u"),
        ("'\\U'", "U"),
    ];

    for (text, expected_value) in escapes.into_iter() {
        let stringified_test = format!("({:?}, {:?}, ..)", text, expected_value);
        let lowered = assert_lower(&format!("a := {}", text));
        assert_eq!(
            literal_value(&lowered),
            &expr::Literal::CharSeq(expected_value.to_string()),
            "At \"{}\"",
            stringified_test
        );
    }
}

#[test]
fn lower_multiple_invalid_char_seq_escapes() {
    assert_eq!(
        literal_value(&assert_lower(r#"a := '\777\ud800\!'"#)),
        &expr::Literal::CharSeq("\u{FFFD}\u{FFFD}!".to_string()),
    );
}

#[test]
fn lower_paren_expr() {
    assert_lower("a := (a)");
    // nested
    assert_lower("a := (((a)))");
    // empty
    assert_lower("a := ()");
}

#[test]
fn lower_self_expr() {
    assert_lower("a := self");
}

#[test]
fn lower_binary_expr() {
    assert_lower("a := a + a");
    // missing operand, should still be present
    assert_lower("a := () + ");
}

#[test]
fn lower_unary_expr() {
    assert_lower("a := + a");
    // missing operand, should still be present
    assert_lower("a := +");
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
        assert_lower(&format!("var _ : {}", ty_text));
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
}

#[test]
fn lower_get_stmt() {
    // single item
    assert_lower("get a*");
    // many items
    assert_lower("get skip, a, b : 1, c : *");
    // no items
    assert_lower("get");
}
