//! Expression testing
use crate::check;
use expect_test::expect;

#[test]
fn report_not_a_stmt() {
    check(
        "pervasive",
        expect![[r##"
            Root@0..9
              Error@0..9
                KwPervasive@0..9 "pervasive"
            error at 0..9: expected ’var’, ’const’, identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’, but found ’pervasive’"##]],
    );
}

#[test]
fn parse_ident_use() {
    check(
        "a",
        expect![[r#"
            Root@0..1
              NameExpr@0..1
                Name@0..1
                  Identifier@0..1 "a""#]],
    );
    check(
        "abcde0123",
        expect![[r#"
            Root@0..9
              NameExpr@0..9
                Name@0..9
                  Identifier@0..9 "abcde0123""#]],
    );
}

#[test]
fn parse_int_literal() {
    check(
        "01234560",
        expect![[r#"
            Root@0..8
              LiteralExpr@0..8
                IntLiteral@0..8 "01234560""#]],
    );

    // TODO: Report invalid literals

    // Overflow, should be detected (in the future)
    check(
        "99999999999999999999",
        expect![[r#"
            Root@0..20
              LiteralExpr@0..20
                IntLiteral@0..20 "99999999999999999999""#]],
    );

    // Digit cutoff
    check(
        "999a999",
        expect![[r#"
            Root@0..7
              LiteralExpr@0..3
                IntLiteral@0..3 "999"
              NameExpr@3..7
                Name@3..7
                  Identifier@3..7 "a999""#]],
    );
}

#[test]
fn parse_radix_literal() {
    // Examples
    check(
        "16#EABC",
        expect![[r##"
            Root@0..7
              LiteralExpr@0..7
                RadixLiteral@0..7 "16#EABC""##]],
    );
    check(
        "02#1100",
        expect![[r##"
            Root@0..7
              LiteralExpr@0..7
                RadixLiteral@0..7 "02#1100""##]],
    );

    // Errors

    // Overflow
    check(
        "10#99999999999999999999",
        expect![[r##"
            Root@0..23
              LiteralExpr@0..23
                RadixLiteral@0..23 "10#99999999999999999999""##]],
    );

    // All errors below here should be reported here
    // TODO: Report invalid literals

    // No digits
    check(
        "30#",
        expect![[r##"
            Root@0..3
              LiteralExpr@0..3
                RadixLiteral@0..3 "30#""##]],
    );

    // Out of range (> 36)
    check(
        "37#asda",
        expect![[r##"
            Root@0..7
              LiteralExpr@0..7
                RadixLiteral@0..7 "37#asda""##]],
    );

    // Out of range (< 2)
    check(
        "0#0000",
        expect![[r##"
            Root@0..6
              LiteralExpr@0..6
                RadixLiteral@0..6 "0#0000""##]],
    );
    check(
        "1#0000",
        expect![[r##"
            Root@0..6
              LiteralExpr@0..6
                RadixLiteral@0..6 "1#0000""##]],
    );

    // Out of range (= overflow)
    check(
        "18446744073709551616#0000",
        expect![[r##"
            Root@0..25
              LiteralExpr@0..25
                RadixLiteral@0..25 "18446744073709551616# ...""##]],
    );

    // Invalid digit
    check(
        "10#999a999",
        expect![[r##"
            Root@0..10
              LiteralExpr@0..10
                RadixLiteral@0..10 "10#999a999""##]],
    );
}

#[test]
fn parse_real_literal() {
    // Leading dot
    check(
        ".12345",
        expect![[r#"
            Root@0..6
              LiteralExpr@0..6
                RealLiteral@0..6 ".12345""#]],
    );
    check(
        ".12345.6789",
        expect![[r#"
            Root@0..11
              LiteralExpr@0..6
                RealLiteral@0..6 ".12345"
              LiteralExpr@6..11
                RealLiteral@6..11 ".6789""#]],
    );

    // Valid variations
    check(
        "1.",
        expect![[r#"
            Root@0..2
              LiteralExpr@0..2
                RealLiteral@0..2 "1.""#]],
    );
    check(
        "100.00",
        expect![[r#"
            Root@0..6
              LiteralExpr@0..6
                RealLiteral@0..6 "100.00""#]],
    );
    check(
        "100.00e10",
        expect![[r#"
            Root@0..9
              LiteralExpr@0..9
                RealLiteral@0..9 "100.00e10""#]],
    );
    check(
        "100.00e100",
        expect![[r#"
            Root@0..10
              LiteralExpr@0..10
                RealLiteral@0..10 "100.00e100""#]],
    );

    // Negative and positive exponents are valid
    check(
        "100.00e-100",
        expect![[r#"
            Root@0..11
              LiteralExpr@0..11
                RealLiteral@0..11 "100.00e-100""#]],
    );
    check(
        "100.00e+100",
        expect![[r#"
            Root@0..11
              LiteralExpr@0..11
                RealLiteral@0..11 "100.00e+100""#]],
    );
    check(
        "1e100",
        expect![[r#"
            Root@0..5
              LiteralExpr@0..5
                RealLiteral@0..5 "1e100""#]],
    );

    // Errors:

    // Invalid format
    check(
        "1e+",
        expect![[r#"
            Root@0..3
              LiteralExpr@0..3
                RealLiteral@0..3 "1e+""#]],
    );
    check(
        "1e-",
        expect![[r#"
            Root@0..3
              LiteralExpr@0..3
                RealLiteral@0..3 "1e-""#]],
    );
    check(
        "1e",
        expect![[r#"
            Root@0..2
              LiteralExpr@0..2
                RealLiteral@0..2 "1e""#]],
    );

    // Too big
    check(
        "1e600",
        expect![[r#"
            Root@0..5
              LiteralExpr@0..5
                RealLiteral@0..5 "1e600""#]],
    );
}

#[test]
fn parse_real_literal_complex_conversions() {
    // Test conversions (all should be valid)

    check("2.225073858507201136057409796709131975934819546351645648023426109724822222021076945516529523908135087914149158913039621106870086438694594645527657207407820621743379988141063267329253552286881372149012981122451451889849057222307285255133155755015914397476397983411801999323962548289017107081850690630666655994938275772572015763062690663332647565300009245888316433037779791869612049497390377829704905051080609940730262937128958950003583799967207254304360284078895771796150945516748243471030702609144621572289880258182545180325707018860872113128079512233426288368622321503775666622503982534335974568884423900265498198385487948292206894721689831099698365846814022854243330660339850886445804001034933970427567186443383770486037861622771738545623065874679014086723327636718749999999999999999999999999999999999999e-308", expect![[r#"
            Root@0..811
              LiteralExpr@0..811
                RealLiteral@0..811 "2.2250738585072011360 ...""#]]);
    check("2.22507385850720113605740979670913197593481954635164564802342610972482222202107694551652952390813508791414915891303962110687008643869459464552765720740782062174337998814106326732925355228688137214901298112245145188984905722230728525513315575501591439747639798341180199932396254828901710708185069063066665599493827577257201576306269066333264756530000924588831643303777979186961204949739037782970490505108060994073026293712895895000358379996720725430436028407889577179615094551674824347103070260914462157228988025818254518032570701886087211312807951223342628836862232150377566662250398253433597456888442390026549819838548794829220689472168983109969836584681402285424333066033985088644580400103493397042756718644338377048603786162277173854562306587467901408672332763671875e-308", expect![[r#"
            Root@0..774
              LiteralExpr@0..774
                RealLiteral@0..774 "2.2250738585072011360 ...""#]]);
    check("0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000222507385850720138309023271733240406421921598046233183055332741688720443481391819585428315901251102056406733973103581100515243416155346010885601238537771882113077799353200233047961014744258363607192156504694250373420837525080665061665815894872049117996859163964850063590877011830487479978088775374994945158045160505091539985658247081864511353793580499211598108576605199243335211435239014879569960959128889160299264151106346631339366347758651302937176204732563178148566435087212282863764204484681140761391147706280168985324411002416144742161856716615054015428508471675290190316132277889672970737312333408698898317506783884692609277397797285865965494109136909540613646756870239867831529068098461721092462539672851562500000000000000001", expect![[r#"
            Root@0..1041
              LiteralExpr@0..1041
                RealLiteral@0..1041 "0.0000000000000000000 ...""#]]);
    check("179769313486231580793728971405303415079934132710037826936173778980444968292764750946649017977587207096330286416692887910946555547851940402630657488671505820681908902000708383676273854845817711531764475730270069855571366959622842914819860834936475292719074168444365510704342711559699508093042880177904174497791.9999999999999999999999999999999999999999999999999999999999999999999999", expect![[r#"
            Root@0..380
              LiteralExpr@0..380
                RealLiteral@0..380 "179769313486231580793 ...""#]]);
    check(
        "2.47032822920623272e-324",
        expect![[r#"
            Root@0..24
              LiteralExpr@0..24
                RealLiteral@0..24 "2.47032822920623272e-324""#]],
    );
    check("6.631236871469758276785396630275967243399099947355303144249971758736286630139265439618068200788048744105960420552601852889715006376325666595539603330361800519107591783233358492337208057849499360899425128640718856616503093444922854759159988160304439909868291973931426625698663157749836252274523485312442358651207051292453083278116143932569727918709786004497872322193856150225415211997283078496319412124640111777216148110752815101775295719811974338451936095907419622417538473679495148632480391435931767981122396703443803335529756003353209830071832230689201383015598792184172909927924176339315507402234836120730914783168400715462440053817592702766213559042115986763819482654128770595766806872783349146967171293949598850675682115696218943412532098591327667236328125E-316", expect![[r#"
            Root@0..766
              LiteralExpr@0..766
                RealLiteral@0..766 "6.6312368714697582767 ...""#]]);
    check("3.237883913302901289588352412501532174863037669423108059901297049552301970670676565786835742587799557860615776559838283435514391084153169252689190564396459577394618038928365305143463955100356696665629202017331344031730044369360205258345803431471660032699580731300954848363975548690010751530018881758184174569652173110473696022749934638425380623369774736560008997404060967498028389191878963968575439222206416981462690113342524002724385941651051293552601421155333430225237291523843322331326138431477823591142408800030775170625915670728657003151953664260769822494937951845801530895238439819708403389937873241463484205608000027270531106827387907791444918534771598750162812548862768493201518991668028251730299953143924168545708663913273994694463908672332763671875E-319", expect![[r#"
            Root@0..763
              LiteralExpr@0..763
                RealLiteral@0..763 "3.2378839133029012895 ...""#]]);
    check("6.953355807847677105972805215521891690222119817145950754416205607980030131549636688806115726399441880065386399864028691275539539414652831584795668560082999889551357784961446896042113198284213107935110217162654939802416034676213829409720583759540476786936413816541621287843248433202369209916612249676005573022703244799714622116542188837770376022371172079559125853382801396219552418839469770514904192657627060319372847562301074140442660237844114174497210955449896389180395827191602886654488182452409583981389442783377001505462015745017848754574668342161759496661766020028752888783387074850773192997102997936619876226688096314989645766000479009083731736585750335262099860150896718774401964796827166283225641992040747894382698751809812609536720628966577351093292236328125E-310", expect![[r#"
            Root@0..772
              LiteralExpr@0..772
                RealLiteral@0..772 "6.9533558078476771059 ...""#]]);
    check("3.339068557571188581835713701280943911923401916998521771655656997328440314559615318168849149074662609099998113009465566426808170378434065722991659642619467706034884424989741080790766778456332168200464651593995817371782125010668346652995912233993254584461125868481633343674905074271064409763090708017856584019776878812425312008812326260363035474811532236853359905334625575404216060622858633280744301892470300555678734689978476870369853549413277156622170245846166991655321535529623870646888786637528995592800436177901746286272273374471701452991433047257863864601424252024791567368195056077320885329384322332391564645264143400798619665040608077549162173963649264049738362290606875883456826586710961041737908872035803481241600376705491726170293986797332763671875E-319", expect![[r#"
            Root@0..763
              LiteralExpr@0..763
                RealLiteral@0..763 "3.3390685575711885818 ...""#]]);
    check("2.4703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772285886546332835517796989819938739800539093906315035659515570226392290858392449105184435931802849936536152500319370457678249219365623669863658480757001585769269903706311928279558551332927834338409351978015531246597263579574622766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968951340535537458516661134223766678604162159680461914467291840300530057530849048765391711386591646239524912623653881879636239373280423891018672348497668235089863388587925628302755995657524455507255189313690836254779186948667994968324049705821028513185451396213837722826145437693412532098591327667236328124999e-324", expect![[r#"
            Root@0..761
              LiteralExpr@0..761
                RealLiteral@0..761 "2.4703282292062327208 ...""#]]);
    check("2.4703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772285886546332835517796989819938739800539093906315035659515570226392290858392449105184435931802849936536152500319370457678249219365623669863658480757001585769269903706311928279558551332927834338409351978015531246597263579574622766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968951340535537458516661134223766678604162159680461914467291840300530057530849048765391711386591646239524912623653881879636239373280423891018672348497668235089863388587925628302755995657524455507255189313690836254779186948667994968324049705821028513185451396213837722826145437693412532098591327667236328125e-324", expect![[r#"
            Root@0..758
              LiteralExpr@0..758
                RealLiteral@0..758 "2.4703282292062327208 ...""#]]);
    check("2.4703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772285886546332835517796989819938739800539093906315035659515570226392290858392449105184435931802849936536152500319370457678249219365623669863658480757001585769269903706311928279558551332927834338409351978015531246597263579574622766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968951340535537458516661134223766678604162159680461914467291840300530057530849048765391711386591646239524912623653881879636239373280423891018672348497668235089863388587925628302755995657524455507255189313690836254779186948667994968324049705821028513185451396213837722826145437693412532098591327667236328125001e-324", expect![[r#"
            Root@0..761
              LiteralExpr@0..761
                RealLiteral@0..761 "2.4703282292062327208 ...""#]]);
    check("7.4109846876186981626485318930233205854758970392148714663837852375101326090531312779794975454245398856969484704316857659638998506553390969459816219401617281718945106978546710679176872575177347315553307795408549809608457500958111373034747658096871009590975442271004757307809711118935784838675653998783503015228055934046593739791790738723868299395818481660169122019456499931289798411362062484498678713572180352209017023903285791732520220528974020802906854021606612375549983402671300035812486479041385743401875520901590172592547146296175134159774938718574737870961645638908718119841271673056017045493004705269590165763776884908267986972573366521765567941072508764337560846003984904972149117463085539556354188641513168478436313080237596295773983001708984374999e-324", expect![[r#"
            Root@0..761
              LiteralExpr@0..761
                RealLiteral@0..761 "7.4109846876186981626 ...""#]]);
    check("7.4109846876186981626485318930233205854758970392148714663837852375101326090531312779794975454245398856969484704316857659638998506553390969459816219401617281718945106978546710679176872575177347315553307795408549809608457500958111373034747658096871009590975442271004757307809711118935784838675653998783503015228055934046593739791790738723868299395818481660169122019456499931289798411362062484498678713572180352209017023903285791732520220528974020802906854021606612375549983402671300035812486479041385743401875520901590172592547146296175134159774938718574737870961645638908718119841271673056017045493004705269590165763776884908267986972573366521765567941072508764337560846003984904972149117463085539556354188641513168478436313080237596295773983001708984375e-324", expect![[r#"
            Root@0..758
              LiteralExpr@0..758
                RealLiteral@0..758 "7.4109846876186981626 ...""#]]);
    check("7.4109846876186981626485318930233205854758970392148714663837852375101326090531312779794975454245398856969484704316857659638998506553390969459816219401617281718945106978546710679176872575177347315553307795408549809608457500958111373034747658096871009590975442271004757307809711118935784838675653998783503015228055934046593739791790738723868299395818481660169122019456499931289798411362062484498678713572180352209017023903285791732520220528974020802906854021606612375549983402671300035812486479041385743401875520901590172592547146296175134159774938718574737870961645638908718119841271673056017045493004705269590165763776884908267986972573366521765567941072508764337560846003984904972149117463085539556354188641513168478436313080237596295773983001708984375001e-324", expect![[r#"
            Root@0..761
              LiteralExpr@0..761
                RealLiteral@0..761 "7.4109846876186981626 ...""#]]);
    check(
        "94393431193180696942841837085033647913224148539854e-358",
        expect![[r#"
            Root@0..55
              LiteralExpr@0..55
                RealLiteral@0..55 "943934311931806969428 ...""#]],
    );
    check("104308485241983990666713401708072175773165034278685682646111762292409330928739751702404658197872319129036519947435319418387839758990478549477777586673075945844895981012024387992135617064532141489278815239849108105951619997829153633535314849999674266169258928940692239684771590065027025835804863585454872499320500023126142553932654370362024104462255244034053203998964360882487378334860197725139151265590832887433736189468858614521708567646743455601905935595381852723723645799866672558576993978025033590728687206296379801363024094048327273913079612469982585674824156000783167963081616214710691759864332339239688734656548790656486646106983450809073750535624894296242072010195710276073042036425579852459556183541199012652571123898996574563824424330960027873516082763671875e-1075", expect![[r#"
            Root@0..774
              LiteralExpr@0..774
                RealLiteral@0..774 "104308485241983990666 ...""#]]);
}

#[test]
fn parse_bin_expr_simple() {
    check(
        "1+2",
        expect![[r#"
            Root@0..3
              BinaryExpr@0..3
                LiteralExpr@0..1
                  IntLiteral@0..1 "1"
                Plus@1..2 "+"
                LiteralExpr@2..3
                  IntLiteral@2..3 "2""#]],
    );
}

#[test]
fn parse_bin_expr_with_left_assoc() {
    check(
        "1+2+3+4",
        expect![[r#"
            Root@0..7
              BinaryExpr@0..7
                BinaryExpr@0..5
                  BinaryExpr@0..3
                    LiteralExpr@0..1
                      IntLiteral@0..1 "1"
                    Plus@1..2 "+"
                    LiteralExpr@2..3
                      IntLiteral@2..3 "2"
                  Plus@3..4 "+"
                  LiteralExpr@4..5
                    IntLiteral@4..5 "3"
                Plus@5..6 "+"
                LiteralExpr@6..7
                  IntLiteral@6..7 "4""#]],
    );
}

#[test]
fn parse_bin_expr_with_mixed_binding_power() {
    check(
        "1+2*3-4/5",
        expect![[r#"
            Root@0..9
              BinaryExpr@0..9
                BinaryExpr@0..5
                  LiteralExpr@0..1
                    IntLiteral@0..1 "1"
                  Plus@1..2 "+"
                  BinaryExpr@2..5
                    LiteralExpr@2..3
                      IntLiteral@2..3 "2"
                    Star@3..4 "*"
                    LiteralExpr@4..5
                      IntLiteral@4..5 "3"
                Minus@5..6 "-"
                BinaryExpr@6..9
                  LiteralExpr@6..7
                    IntLiteral@6..7 "4"
                  Slash@7..8 "/"
                  LiteralExpr@8..9
                    IntLiteral@8..9 "5""#]],
    );
}

#[test]
fn parse_expr_with_leading_ws() {
    check(
        "     16#480",
        expect![[r##"
            Root@0..11
              Whitespace@0..5 "     "
              LiteralExpr@5..11
                RadixLiteral@5..11 "16#480""##]],
    );
}

#[test]
fn parse_expr_with_trailing_ws() {
    check(
        "1.0e5    ",
        expect![[r#"
            Root@0..9
              LiteralExpr@0..9
                RealLiteral@0..5 "1.0e5"
                Whitespace@5..9 "    ""#]],
    );
}

#[test]
fn parse_expr_with_surrounding_ws() {
    check(
        "  12345    ",
        expect![[r#"
            Root@0..11
              Whitespace@0..2 "  "
              LiteralExpr@2..11
                IntLiteral@2..7 "12345"
                Whitespace@7..11 "    ""#]],
    );
}

#[test]
fn parse_bin_expr_with_ws() {
    check(
        "  1 + 2 - 3* 4    ",
        expect![[r#"
            Root@0..18
              Whitespace@0..2 "  "
              BinaryExpr@2..18
                BinaryExpr@2..8
                  LiteralExpr@2..4
                    IntLiteral@2..3 "1"
                    Whitespace@3..4 " "
                  Plus@4..5 "+"
                  Whitespace@5..6 " "
                  LiteralExpr@6..8
                    IntLiteral@6..7 "2"
                    Whitespace@7..8 " "
                Minus@8..9 "-"
                Whitespace@9..10 " "
                BinaryExpr@10..18
                  LiteralExpr@10..11
                    IntLiteral@10..11 "3"
                  Star@11..12 "*"
                  Whitespace@12..13 " "
                  LiteralExpr@13..18
                    IntLiteral@13..14 "4"
                    Whitespace@14..18 "    ""#]],
    );
}

#[test]
fn parse_exprs_with_comments() {
    check(
        r#"
        1
        + 2 % interspersed line comment
        + /* random interleaved comment */ 3

        3 - 2 % step down two
        + 1 % go back up 1"#,
        expect![[r#"
            Root@0..153
              Whitespace@0..9 "\n        "
              BinaryExpr@9..105
                BinaryExpr@9..59
                  LiteralExpr@9..19
                    IntLiteral@9..10 "1"
                    Whitespace@10..19 "\n        "
                  Plus@19..20 "+"
                  Whitespace@20..21 " "
                  LiteralExpr@21..59
                    IntLiteral@21..22 "2"
                    Whitespace@22..23 " "
                    Comment@23..50 "% interspersed line c ..."
                    Whitespace@50..59 "\n        "
                Plus@59..60 "+"
                Whitespace@60..61 " "
                Comment@61..93 "/* random interleaved ..."
                Whitespace@93..94 " "
                LiteralExpr@94..105
                  IntLiteral@94..95 "3"
                  Whitespace@95..105 "\n\n        "
              BinaryExpr@105..153
                BinaryExpr@105..135
                  LiteralExpr@105..107
                    IntLiteral@105..106 "3"
                    Whitespace@106..107 " "
                  Minus@107..108 "-"
                  Whitespace@108..109 " "
                  LiteralExpr@109..135
                    IntLiteral@109..110 "2"
                    Whitespace@110..111 " "
                    Comment@111..126 "% step down two"
                    Whitespace@126..135 "\n        "
                Plus@135..136 "+"
                Whitespace@136..137 " "
                LiteralExpr@137..153
                  IntLiteral@137..138 "1"
                  Whitespace@138..139 " "
                  Comment@139..153 "% go back up 1""#]],
    );
}

#[test]
fn parse_simple_infix() {
    check(
        "1 => 2",
        expect![[r#"
            Root@0..6
              BinaryExpr@0..6
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                Imply@2..4 "=>"
                Whitespace@4..5 " "
                LiteralExpr@5..6
                  IntLiteral@5..6 "2""#]],
    );
    check(
        "1 or 2",
        expect![[r#"
            Root@0..6
              BinaryExpr@0..6
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                KwOr@2..4 "or"
                Whitespace@4..5 " "
                LiteralExpr@5..6
                  IntLiteral@5..6 "2""#]],
    );
    check(
        "1 and 2",
        expect![[r#"
            Root@0..7
              BinaryExpr@0..7
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                KwAnd@2..5 "and"
                Whitespace@5..6 " "
                LiteralExpr@6..7
                  IntLiteral@6..7 "2""#]],
    );
    check(
        "1 < 2",
        expect![[r#"
            Root@0..5
              BinaryExpr@0..5
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                Less@2..3 "<"
                Whitespace@3..4 " "
                LiteralExpr@4..5
                  IntLiteral@4..5 "2""#]],
    );
    check(
        "1 > 2",
        expect![[r#"
            Root@0..5
              BinaryExpr@0..5
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                Greater@2..3 ">"
                Whitespace@3..4 " "
                LiteralExpr@4..5
                  IntLiteral@4..5 "2""#]],
    );
    check(
        "1 <= 2",
        expect![[r#"
            Root@0..6
              BinaryExpr@0..6
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                LessEqu@2..4 "<="
                Whitespace@4..5 " "
                LiteralExpr@5..6
                  IntLiteral@5..6 "2""#]],
    );
    check(
        "1 >= 2",
        expect![[r#"
            Root@0..6
              BinaryExpr@0..6
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                GreaterEqu@2..4 ">="
                Whitespace@4..5 " "
                LiteralExpr@5..6
                  IntLiteral@5..6 "2""#]],
    );
    check(
        "1 + 2",
        expect![[r#"
            Root@0..5
              BinaryExpr@0..5
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                Plus@2..3 "+"
                Whitespace@3..4 " "
                LiteralExpr@4..5
                  IntLiteral@4..5 "2""#]],
    );
    check(
        "1 - 2",
        expect![[r#"
            Root@0..5
              BinaryExpr@0..5
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                Minus@2..3 "-"
                Whitespace@3..4 " "
                LiteralExpr@4..5
                  IntLiteral@4..5 "2""#]],
    );
    check(
        "1 * 2",
        expect![[r#"
            Root@0..5
              BinaryExpr@0..5
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                Star@2..3 "*"
                Whitespace@3..4 " "
                LiteralExpr@4..5
                  IntLiteral@4..5 "2""#]],
    );
    check(
        "1 / 2",
        expect![[r#"
            Root@0..5
              BinaryExpr@0..5
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                Slash@2..3 "/"
                Whitespace@3..4 " "
                LiteralExpr@4..5
                  IntLiteral@4..5 "2""#]],
    );
    check(
        "1 div 2",
        expect![[r#"
            Root@0..7
              BinaryExpr@0..7
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                KwDiv@2..5 "div"
                Whitespace@5..6 " "
                LiteralExpr@6..7
                  IntLiteral@6..7 "2""#]],
    );
    check(
        "1 rem 2",
        expect![[r#"
            Root@0..7
              BinaryExpr@0..7
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                KwRem@2..5 "rem"
                Whitespace@5..6 " "
                LiteralExpr@6..7
                  IntLiteral@6..7 "2""#]],
    );
    check(
        "1 mod 2",
        expect![[r#"
            Root@0..7
              BinaryExpr@0..7
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                KwMod@2..5 "mod"
                Whitespace@5..6 " "
                LiteralExpr@6..7
                  IntLiteral@6..7 "2""#]],
    );
    check(
        "1 shl 2",
        expect![[r#"
            Root@0..7
              BinaryExpr@0..7
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                KwShl@2..5 "shl"
                Whitespace@5..6 " "
                LiteralExpr@6..7
                  IntLiteral@6..7 "2""#]],
    );
    check(
        "1 shr 2",
        expect![[r#"
            Root@0..7
              BinaryExpr@0..7
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                KwShr@2..5 "shr"
                Whitespace@5..6 " "
                LiteralExpr@6..7
                  IntLiteral@6..7 "2""#]],
    );
    check(
        "1 ** 2",
        expect![[r#"
            Root@0..6
              BinaryExpr@0..6
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                Exp@2..4 "**"
                Whitespace@4..5 " "
                LiteralExpr@5..6
                  IntLiteral@5..6 "2""#]],
    );
    // call, dot, and arrow are complex operators
    // not in and not eq are compound infix
}

#[test]
fn exp_right_associativity() {
    check(
        "2 ** 3 ** 4",
        expect![[r#"
            Root@0..11
              BinaryExpr@0..11
                LiteralExpr@0..2
                  IntLiteral@0..1 "2"
                  Whitespace@1..2 " "
                Exp@2..4 "**"
                Whitespace@4..5 " "
                BinaryExpr@5..11
                  LiteralExpr@5..7
                    IntLiteral@5..6 "3"
                    Whitespace@6..7 " "
                  Exp@7..9 "**"
                  Whitespace@9..10 " "
                  LiteralExpr@10..11
                    IntLiteral@10..11 "4""#]],
    );
}

#[test]
fn parse_ne_form1() {
    check(
        "1 ~= 2",
        expect![[r#"
            Root@0..6
              BinaryExpr@0..6
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                NotEq@2..5
                  Tilde@2..3 "~"
                  Equ@3..4 "="
                  Whitespace@4..5 " "
                LiteralExpr@5..6
                  IntLiteral@5..6 "2""#]],
    );
}

#[test]
fn parse_ne_form2() {
    check(
        "1 ~ = 2",
        expect![[r#"
            Root@0..7
              BinaryExpr@0..7
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                NotEq@2..6
                  Tilde@2..3 "~"
                  Whitespace@3..4 " "
                  Equ@4..5 "="
                  Whitespace@5..6 " "
                LiteralExpr@6..7
                  IntLiteral@6..7 "2""#]],
    );
}

#[test]
fn parse_ne_form3() {
    check(
        "1 not = 2",
        expect![[r#"
            Root@0..9
              BinaryExpr@0..9
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                NotEq@2..8
                  KwNot@2..5 "not"
                  Whitespace@5..6 " "
                  Equ@6..7 "="
                  Whitespace@7..8 " "
                LiteralExpr@8..9
                  IntLiteral@8..9 "2""#]],
    );
}

#[test]
fn parse_ne_form4() {
    check(
        "1 not= 2",
        expect![[r#"
            Root@0..8
              BinaryExpr@0..8
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                NotEq@2..7
                  KwNot@2..5 "not"
                  Equ@5..6 "="
                  Whitespace@6..7 " "
                LiteralExpr@7..8
                  IntLiteral@7..8 "2""#]],
    );
}

#[test]
fn parse_in() {
    check(
        "1 in a",
        expect![[r#"
            Root@0..6
              BinaryExpr@0..6
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                KwIn@2..4 "in"
                Whitespace@4..5 " "
                NameExpr@5..6
                  Name@5..6
                    Identifier@5..6 "a""#]],
    );
}

#[test]
fn parse_not_in_form1() {
    check(
        "1 not in a",
        expect![[r#"
            Root@0..10
              BinaryExpr@0..10
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                NotIn@2..9
                  KwNot@2..5 "not"
                  Whitespace@5..6 " "
                  KwIn@6..8 "in"
                  Whitespace@8..9 " "
                NameExpr@9..10
                  Name@9..10
                    Identifier@9..10 "a""#]],
    );
}

#[test]
fn parse_not_in_form2() {
    check(
        "1 ~in a",
        expect![[r#"
            Root@0..7
              BinaryExpr@0..7
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                NotIn@2..6
                  Tilde@2..3 "~"
                  KwIn@3..5 "in"
                  Whitespace@5..6 " "
                NameExpr@6..7
                  Name@6..7
                    Identifier@6..7 "a""#]],
    );
}

#[test]
fn parse_not_in_form3() {
    check(
        "1 ~ in a",
        expect![[r#"
            Root@0..8
              BinaryExpr@0..8
                LiteralExpr@0..2
                  IntLiteral@0..1 "1"
                  Whitespace@1..2 " "
                NotIn@2..7
                  Tilde@2..3 "~"
                  Whitespace@3..4 " "
                  KwIn@4..6 "in"
                  Whitespace@6..7 " "
                NameExpr@7..8
                  Name@7..8
                    Identifier@7..8 "a""#]],
    );
}

#[test]
fn recover_tilde_as_infix() {
    check(
        "1 ~ 2",
        expect![[r#"
            Root@0..5
              LiteralExpr@0..2
                IntLiteral@0..1 "1"
                Whitespace@1..2 " "
              Error@2..5
                Tilde@2..3 "~"
                Whitespace@3..4 " "
                IntLiteral@4..5 "2"
            error at 4..5: expected ’in’ or ’=’, but found int literal"#]],
    );
}

#[test]
fn recover_tilde_not_infix() {
    check(
        "1 not 2",
        expect![[r#"
            Root@0..7
              LiteralExpr@0..2
                IntLiteral@0..1 "1"
                Whitespace@1..2 " "
              Error@2..7
                KwNot@2..5 "not"
                Whitespace@5..6 " "
                IntLiteral@6..7 "2"
            error at 6..7: expected ’in’ or ’=’, but found int literal"#]],
    );
}

#[test]
fn parse_simple_prefix() {
    check(
        "-10",
        expect![[r#"
            Root@0..3
              UnaryExpr@0..3
                Minus@0..1 "-"
                LiteralExpr@1..3
                  IntLiteral@1..3 "10""#]],
    );
}

#[test]
fn negation_over_arithmetic() {
    check(
        "-10+20",
        expect![[r#"
            Root@0..6
              BinaryExpr@0..6
                UnaryExpr@0..3
                  Minus@0..1 "-"
                  LiteralExpr@1..3
                    IntLiteral@1..3 "10"
                Plus@3..4 "+"
                LiteralExpr@4..6
                  IntLiteral@4..6 "20""#]],
    );
}

#[test]
fn parse_nested_parens() {
    check(
        "(((20)))",
        expect![[r#"
            Root@0..8
              ParenExpr@0..8
                LeftParen@0..1 "("
                ParenExpr@1..7
                  LeftParen@1..2 "("
                  ParenExpr@2..6
                    LeftParen@2..3 "("
                    LiteralExpr@3..5
                      IntLiteral@3..5 "20"
                    RightParen@5..6 ")"
                  RightParen@6..7 ")"
                RightParen@7..8 ")""#]],
    );
}

#[test]
fn parens_alter_precedence() {
    check(
        "1/(2+3)",
        expect![[r#"
            Root@0..7
              BinaryExpr@0..7
                LiteralExpr@0..1
                  IntLiteral@0..1 "1"
                Slash@1..2 "/"
                ParenExpr@2..7
                  LeftParen@2..3 "("
                  BinaryExpr@3..6
                    LiteralExpr@3..4
                      IntLiteral@3..4 "2"
                    Plus@4..5 "+"
                    LiteralExpr@5..6
                      IntLiteral@5..6 "3"
                  RightParen@6..7 ")""#]],
    );
}

#[test]
fn recover_missing_closing_paren() {
    check(
        "(1",
        expect![[r#"
            Root@0..2
              ParenExpr@0..2
                LeftParen@0..1 "("
                LiteralExpr@1..2
                  IntLiteral@1..2 "1"
            error at 1..2: expected ’)’"#]],
    );
}

#[test]
fn recover_missing_closing_paren_and_rhs() {
    check(
        "(1+",
        expect![[r##"
            Root@0..3
              ParenExpr@0..3
                LeftParen@0..1 "("
                BinaryExpr@1..3
                  LiteralExpr@1..2
                    IntLiteral@1..2 "1"
                  Plus@2..3 "+"
            error at 2..3: expected identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’
            error at 2..3: expected ’)’"##]],
    );
}

#[test]
fn recover_missing_rhs() {
    check(
        "1+",
        expect![[r##"
            Root@0..2
              BinaryExpr@0..2
                LiteralExpr@0..1
                  IntLiteral@0..1 "1"
                Plus@1..2 "+"
            error at 1..2: expected identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’"##]],
    );
}

// Field exprs

#[test]
fn parse_field_expr() {
    check(
        "a.b",
        expect![[r#"
            Root@0..3
              FieldExpr@0..3
                NameExpr@0..1
                  Name@0..1
                    Identifier@0..1 "a"
                Dot@1..2 "."
                Name@2..3
                  Identifier@2..3 "b""#]],
    );
}

#[test]
fn chained_field_expr() {
    check(
        "a.b.c.d",
        expect![[r#"
            Root@0..7
              FieldExpr@0..7
                FieldExpr@0..5
                  FieldExpr@0..3
                    NameExpr@0..1
                      Name@0..1
                        Identifier@0..1 "a"
                    Dot@1..2 "."
                    Name@2..3
                      Identifier@2..3 "b"
                  Dot@3..4 "."
                  Name@4..5
                    Identifier@4..5 "c"
                Dot@5..6 "."
                Name@6..7
                  Identifier@6..7 "d""#]],
    );
}

#[test]
fn recover_field_expr_missing_field() {
    check(
        "a.",
        expect![[r#"
            Root@0..2
              FieldExpr@0..2
                NameExpr@0..1
                  Name@0..1
                    Identifier@0..1 "a"
                Dot@1..2 "."
            error at 1..2: expected identifier"#]],
    );
}

#[test]
fn recover_field_missing_closing_paren_and_field() {
    check(
        "(a.",
        expect![[r#"
            Root@0..3
              ParenExpr@0..3
                LeftParen@0..1 "("
                FieldExpr@1..3
                  NameExpr@1..2
                    Name@1..2
                      Identifier@1..2 "a"
                  Dot@2..3 "."
            error at 2..3: expected identifier
            error at 2..3: expected ’)’"#]],
    );
}

// Arrow exprs

#[test]
fn parse_arrow_expr() {
    check(
        "a->b",
        expect![[r#"
            Root@0..4
              ArrowExpr@0..4
                NameExpr@0..1
                  Name@0..1
                    Identifier@0..1 "a"
                Arrow@1..3 "->"
                Name@3..4
                  Identifier@3..4 "b""#]],
    );
}

#[test]
fn chained_arrow_expr() {
    check(
        "a->b->c->d",
        expect![[r#"
            Root@0..10
              ArrowExpr@0..10
                ArrowExpr@0..7
                  ArrowExpr@0..4
                    NameExpr@0..1
                      Name@0..1
                        Identifier@0..1 "a"
                    Arrow@1..3 "->"
                    Name@3..4
                      Identifier@3..4 "b"
                  Arrow@4..6 "->"
                  Name@6..7
                    Identifier@6..7 "c"
                Arrow@7..9 "->"
                Name@9..10
                  Identifier@9..10 "d""#]],
    );
}

#[test]
fn recover_arrow_expr_missing_field() {
    check(
        "a->",
        expect![[r#"
            Root@0..3
              ArrowExpr@0..3
                NameExpr@0..1
                  Name@0..1
                    Identifier@0..1 "a"
                Arrow@1..3 "->"
            error at 1..3: expected identifier"#]],
    );
}

#[test]
fn recover_arrow_missing_closing_paren_and_field() {
    check(
        "(a->",
        expect![[r#"
            Root@0..4
              ParenExpr@0..4
                LeftParen@0..1 "("
                ArrowExpr@1..4
                  NameExpr@1..2
                    Name@1..2
                      Identifier@1..2 "a"
                  Arrow@2..4 "->"
            error at 2..4: expected identifier
            error at 2..4: expected ’)’"#]],
    );
}

#[test]
fn chained_field_and_arrow_expr() {
    check(
        "a.b->c->d.e->f.g->h",
        expect![[r#"
            Root@0..19
              ArrowExpr@0..19
                FieldExpr@0..16
                  ArrowExpr@0..14
                    FieldExpr@0..11
                      ArrowExpr@0..9
                        ArrowExpr@0..6
                          FieldExpr@0..3
                            NameExpr@0..1
                              Name@0..1
                                Identifier@0..1 "a"
                            Dot@1..2 "."
                            Name@2..3
                              Identifier@2..3 "b"
                          Arrow@3..5 "->"
                          Name@5..6
                            Identifier@5..6 "c"
                        Arrow@6..8 "->"
                        Name@8..9
                          Identifier@8..9 "d"
                      Dot@9..10 "."
                      Name@10..11
                        Identifier@10..11 "e"
                    Arrow@11..13 "->"
                    Name@13..14
                      Identifier@13..14 "f"
                  Dot@14..15 "."
                  Name@15..16
                    Identifier@15..16 "g"
                Arrow@16..18 "->"
                Name@18..19
                  Identifier@18..19 "h""#]],
    );
}

// Call exprs
#[test]
fn parse_call_expr() {
    check(
        "a(b)",
        expect![[r#"
            Root@0..4
              CallExpr@0..4
                NameExpr@0..1
                  Name@0..1
                    Identifier@0..1 "a"
                LeftParen@1..2 "("
                ParamList@2..3
                  Param@2..3
                    NameExpr@2..3
                      Name@2..3
                        Identifier@2..3 "b"
                RightParen@3..4 ")""#]],
    );
}

#[test]
fn parse_empty_call_expr() {
    check(
        "a()",
        expect![[r##"
            Root@0..3
              CallExpr@0..3
                NameExpr@0..1
                  Name@0..1
                    Identifier@0..1 "a"
                LeftParen@1..2 "("
                ParamList@2..3
                  Param@2..3
                    Error@2..3
                      RightParen@2..3 ")"
            error at 2..3: expected identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’, but found ’)’
            error at 2..3: expected ’,’ or ’)’"##]],
    );
}

#[test]
fn parse_call_expr_with_many_args() {
    check(
        "a(1, 2 + 3, c)",
        expect![[r#"
            Root@0..14
              CallExpr@0..14
                NameExpr@0..1
                  Name@0..1
                    Identifier@0..1 "a"
                LeftParen@1..2 "("
                ParamList@2..13
                  Param@2..5
                    LiteralExpr@2..3
                      IntLiteral@2..3 "1"
                    Comma@3..4 ","
                    Whitespace@4..5 " "
                  Param@5..12
                    BinaryExpr@5..10
                      LiteralExpr@5..7
                        IntLiteral@5..6 "2"
                        Whitespace@6..7 " "
                      Plus@7..8 "+"
                      Whitespace@8..9 " "
                      LiteralExpr@9..10
                        IntLiteral@9..10 "3"
                    Comma@10..11 ","
                    Whitespace@11..12 " "
                  Param@12..13
                    NameExpr@12..13
                      Name@12..13
                        Identifier@12..13 "c"
                RightParen@13..14 ")""#]],
    );
}

#[test]
fn recover_call_expr_missing_closing_paren() {
    check(
        "a(1",
        expect![[r#"
            Root@0..3
              CallExpr@0..3
                NameExpr@0..1
                  Name@0..1
                    Identifier@0..1 "a"
                LeftParen@1..2 "("
                ParamList@2..3
                  Param@2..3
                    LiteralExpr@2..3
                      IntLiteral@2..3 "1"
            error at 2..3: expected ’,’ or ’)’"#]],
    );
}

#[test]
fn recover_call_expr_missing_last_arg() {
    check(
        "a(1,)",
        expect![[r##"
            Root@0..5
              CallExpr@0..5
                NameExpr@0..1
                  Name@0..1
                    Identifier@0..1 "a"
                LeftParen@1..2 "("
                ParamList@2..5
                  Param@2..4
                    LiteralExpr@2..3
                      IntLiteral@2..3 "1"
                    Comma@3..4 ","
                  Param@4..5
                    Error@4..5
                      RightParen@4..5 ")"
            error at 4..5: expected identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’, but found ’)’
            error at 4..5: expected ’,’ or ’)’"##]],
    );
}

#[test]
fn recover_call_expr_missing_last_arg_and_closing_paren() {
    check(
        "a(1,",
        expect![[r##"
            Root@0..4
              CallExpr@0..4
                NameExpr@0..1
                  Name@0..1
                    Identifier@0..1 "a"
                LeftParen@1..2 "("
                ParamList@2..4
                  Param@2..4
                    LiteralExpr@2..3
                      IntLiteral@2..3 "1"
                    Comma@3..4 ","
                  Param@4..4
            error at 3..4: expected identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’
            error at 3..4: expected ’,’ or ’)’"##]],
    );
}

#[test]
fn recover_call_expr_missing_delim() {
    check(
        "a(1 1)",
        expect![[r##"
            Root@0..6
              CallExpr@0..5
                NameExpr@0..1
                  Name@0..1
                    Identifier@0..1 "a"
                LeftParen@1..2 "("
                ParamList@2..4
                  Param@2..4
                    LiteralExpr@2..4
                      IntLiteral@2..3 "1"
                      Whitespace@3..4 " "
                Error@4..5
                  IntLiteral@4..5 "1"
              Error@5..6
                RightParen@5..6 ")"
            error at 4..5: expected ’,’ or ’)’, but found int literal
            error at 5..6: expected ’var’, ’const’, identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’, but found ’)’"##]],
    );
}

// Deref exprs

#[test]
fn parse_deref_expr() {
    check(
        "^a",
        expect![[r#"
            Root@0..2
              DerefExpr@0..2
                Caret@0..1 "^"
                NameExpr@1..2
                  Name@1..2
                    Identifier@1..2 "a""#]],
    );
}

#[test]
fn nested_deref() {
    check(
        "^ ^ ^ ^ ^ a",
        expect![[r#"
            Root@0..11
              DerefExpr@0..11
                Caret@0..1 "^"
                Whitespace@1..2 " "
                DerefExpr@2..11
                  Caret@2..3 "^"
                  Whitespace@3..4 " "
                  DerefExpr@4..11
                    Caret@4..5 "^"
                    Whitespace@5..6 " "
                    DerefExpr@6..11
                      Caret@6..7 "^"
                      Whitespace@7..8 " "
                      DerefExpr@8..11
                        Caret@8..9 "^"
                        Whitespace@9..10 " "
                        NameExpr@10..11
                          Name@10..11
                            Identifier@10..11 "a""#]],
    );
}

#[test]
fn deref_binds_higher_than_dot() {
    check(
        "^a.b",
        expect![[r#"
            Root@0..4
              FieldExpr@0..4
                DerefExpr@0..2
                  Caret@0..1 "^"
                  NameExpr@1..2
                    Name@1..2
                      Identifier@1..2 "a"
                Dot@2..3 "."
                Name@3..4
                  Identifier@3..4 "b""#]],
    );
}

#[test]
fn deref_binds_higher_than_arrow() {
    check(
        "^a.b",
        expect![[r#"
            Root@0..4
              FieldExpr@0..4
                DerefExpr@0..2
                  Caret@0..1 "^"
                  NameExpr@1..2
                    Name@1..2
                      Identifier@1..2 "a"
                Dot@2..3 "."
                Name@3..4
                  Identifier@3..4 "b""#]],
    );
}

#[test]
fn deref_binds_higher_than_call() {
    check(
        "^a()",
        expect![[r##"
            Root@0..4
              CallExpr@0..4
                DerefExpr@0..2
                  Caret@0..1 "^"
                  NameExpr@1..2
                    Name@1..2
                      Identifier@1..2 "a"
                LeftParen@2..3 "("
                ParamList@3..4
                  Param@3..4
                    Error@3..4
                      RightParen@3..4 ")"
            error at 3..4: expected identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’, but found ’)’
            error at 3..4: expected ’,’ or ’)’"##]],
    );
}
