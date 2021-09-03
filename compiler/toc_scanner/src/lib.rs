//! Scanner for lexing tokens
pub mod token;

use logos::Logos;
use std::ops::Range;
use toc_reporting::{MessageBundle, MessageSink};
use toc_span::{FileId, Span};
use token::{NumberKind, Token, TokenKind};

#[derive(Debug, Default)]
pub struct ErrorFerry {
    file_id: Option<FileId>,
    sink: MessageSink,
}

impl ErrorFerry {
    pub(crate) fn push_error(&mut self, message: &str, at_span: &str, span: Range<usize>) {
        let range = token::span_to_text_range(span);

        self.sink
            .error(message, at_span, Span::new(self.file_id, range));
    }

    pub(crate) fn finish(self) -> MessageBundle {
        self.sink.finish()
    }
}

/// Scanner for tokens
pub struct Scanner<'s> {
    inner: logos::Lexer<'s, TokenKind>,
}

impl<'s> Scanner<'s> {
    pub fn new(file: Option<FileId>, source: &'s str) -> Self {
        let mut inner = TokenKind::lexer(source);
        // Set up the file id
        inner.extras.file_id = file;

        Self { inner }
    }

    pub fn collect_all(mut self) -> (Vec<Token<'s>>, MessageBundle) {
        let mut toks = vec![];

        for token in &mut self {
            toks.push(token)
        }

        (toks, self.inner.extras.finish())
    }

    fn next_token(&mut self) -> Option<Token<'s>> {
        let kind = match self.inner.next()? {
            TokenKind::NumberLiteral(number_kind) => match number_kind {
                NumberKind::Int => TokenKind::IntLiteral,
                NumberKind::Radix => TokenKind::RadixLiteral,
                NumberKind::Real => TokenKind::RealLiteral,
            },
            TokenKind::Error => {
                // Report the invalid character
                self.inner.extras.push_error(
                    "invalid character",
                    "", // intentionally left empty, message says it all
                    self.inner.span(),
                );
                TokenKind::Error
            }
            other => other,
        };

        let text = self.inner.slice();
        let range = token::span_to_text_range(self.inner.span());

        Some(Token::new(kind, text, range))
    }
}

impl<'s> std::iter::Iterator for Scanner<'s> {
    type Item = Token<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use expect_test::{expect, Expect};

    fn do_scanner(source: &str) -> (Vec<(TokenKind, &str)>, String) {
        let scanner = Scanner::new(None, source);

        let (toks, errors) = scanner.collect_all();
        let toks: Vec<(TokenKind, &str)> =
            toks.into_iter().map(|tok| (tok.kind, tok.lexeme)).collect();
        let errors = build_error_list(errors);

        (toks, errors)
    }

    fn build_error_list(errors: MessageBundle) -> String {
        let mut buf = String::new();

        for msg in errors.iter() {
            buf.push_str(&msg.to_string());
        }
        buf.pop();

        buf
    }

    /// Runs the lexer over the given text, and asserts if the expected token
    /// has been scanned
    ///
    /// Expects no errors to be produced
    #[track_caller] // Issue isn't here, but from caller
    fn expect(source: &str, kind: &TokenKind) {
        let mut scanner = Scanner::new(None, source);

        // Should be the expected token & the same source text
        let token = scanner.next().unwrap();
        assert_eq!(token.kind, *kind);
        assert_eq!(token.lexeme, source);
        assert_eq!(build_error_list(scanner.inner.extras.finish()), "");
    }

    /// Runs the lexer over the given text, and asserts if the expected tokens
    /// have been scanned
    ///
    /// Expects no errors to be produced
    #[track_caller] // Issue isn't here, but from caller
    fn expect_seq(source: &str, kinds: &[(TokenKind, &str)]) {
        let (toks, errors) = do_scanner(source);
        assert_eq!(toks.is_empty(), kinds.is_empty());
        assert_eq!(toks, kinds);
        assert_eq!(errors, "");
    }

    #[track_caller] // Issue isn't here, but from caller
    fn expect_seq_with_errors(source: &str, kinds: &[(TokenKind, &str)], expecting: Expect) {
        let (toks, errors) = do_scanner(source);
        assert_eq!(toks.is_empty(), kinds.is_empty());
        assert_eq!(toks, kinds);
        expecting.assert_eq(&errors);
    }

    #[track_caller]
    fn expect_with_error(source: &str, kind: &TokenKind, expecting: Expect) {
        let mut scanner = Scanner::new(None, source);

        // Should be the expected token & the same source text
        let token = scanner.next().unwrap();
        assert_eq!(token.kind, *kind);
        assert_eq!(token.lexeme, source);

        let buf = build_error_list(scanner.inner.extras.finish());

        expecting.assert_eq(&buf);
    }

    #[test]
    fn scan_whitespace() {
        expect("     ", &TokenKind::Whitespace);
        expect("     \t", &TokenKind::Whitespace);
        expect("   \n   ", &TokenKind::Whitespace);
        expect("   \r\n   ", &TokenKind::Whitespace);
    }

    #[test]
    fn scan_invalid_chars() {
        // Lossless scanning
        expect_with_error(
            "[",
            &TokenKind::Error,
            expect![[r#"error at 0..1: invalid character"#]],
        );
        expect_with_error(
            "]",
            &TokenKind::Error,
            expect![[r#"error at 0..1: invalid character"#]],
        );
        expect_with_error(
            "{",
            &TokenKind::Error,
            expect![[r#"error at 0..1: invalid character"#]],
        );
        expect_with_error(
            "}",
            &TokenKind::Error,
            expect![[r#"error at 0..1: invalid character"#]],
        );
        expect_with_error(
            "!",
            &TokenKind::Error,
            expect![[r#"error at 0..1: invalid character"#]],
        );
        expect_with_error(
            "$",
            &TokenKind::Error,
            expect![[r#"error at 0..1: invalid character"#]],
        );
        expect_with_error(
            "?",
            &TokenKind::Error,
            expect![[r#"error at 0..1: invalid character"#]],
        );
        expect_with_error(
            "`",
            &TokenKind::Error,
            expect![[r#"error at 0..1: invalid character"#]],
        );
        expect_with_error(
            "\\",
            &TokenKind::Error,
            expect![[r#"error at 0..1: invalid character"#]],
        );
        // Was originally `🧑‍🔬` but that gets split up into multiple tokens
        expect_with_error(
            "🧑",
            &TokenKind::Error,
            expect![[r#"error at 0..4: invalid character"#]],
        );
    }

    #[test]
    #[ignore = "no normalization planned yet for identifiers"]
    fn text_normalization() {
        // Normalization issues: `â` and `â` are distinct
        // Text should be normalized, ie both should be characters
        expect("â", &TokenKind::Error);
        expect("â", &TokenKind::Error);
    }

    #[test]
    fn scan_real_literals() {
        // Allow for leading dot
        expect(".12345", &TokenKind::RealLiteral);
        expect_seq(
            ".12345.6789",
            &[
                (TokenKind::RealLiteral, ".12345"),
                (TokenKind::RealLiteral, ".6789"),
            ],
        );

        expect("1.", &TokenKind::RealLiteral);
        expect("100.00", &TokenKind::RealLiteral);
        expect("100.00e10", &TokenKind::RealLiteral);
        expect("100.00e100", &TokenKind::RealLiteral);

        // Negative and positive exponents are valid
        expect("100.00e-100", &TokenKind::RealLiteral);
        expect("100.00e+100", &TokenKind::RealLiteral);
        expect("1e100", &TokenKind::RealLiteral);

        // No errors for invalid literals are reported here

        // Invalid format
        expect("1e+", &TokenKind::RealLiteral);
        expect("1e-", &TokenKind::RealLiteral);
        expect("1e", &TokenKind::RealLiteral);
        expect("1.0e", &TokenKind::RealLiteral);

        // Don't consume extra '+', '-' or decimal digits
        expect_seq(
            "1.0+",
            &[(TokenKind::RealLiteral, "1.0"), (TokenKind::Plus, "+")],
        );
        expect_seq(
            "1.0-",
            &[(TokenKind::RealLiteral, "1.0"), (TokenKind::Minus, "-")],
        );
        expect_seq(
            "1.0-1000",
            &[
                (TokenKind::RealLiteral, "1.0"),
                (TokenKind::Minus, "-"),
                (TokenKind::IntLiteral, "1000"),
            ],
        );

        // Too big
        expect("1e600", &TokenKind::RealLiteral);
        expect("1.0e600", &TokenKind::RealLiteral);

        // Too small (should not produce an error)
        expect("1e-999999999", &TokenKind::RealLiteral);
        expect("1.0e-999999999", &TokenKind::RealLiteral);
    }

    #[test]
    fn scan_complex_real_literals() {
        // Test conversions (all should be valid)
        expect_with_error("2.225073858507201136057409796709131975934819546351645648023426109724822222021076945516529523908135087914149158913039621106870086438694594645527657207407820621743379988141063267329253552286881372149012981122451451889849057222307285255133155755015914397476397983411801999323962548289017107081850690630666655994938275772572015763062690663332647565300009245888316433037779791869612049497390377829704905051080609940730262937128958950003583799967207254304360284078895771796150945516748243471030702609144621572289880258182545180325707018860872113128079512233426288368622321503775666622503982534335974568884423900265498198385487948292206894721689831099698365846814022854243330660339850886445804001034933970427567186443383770486037861622771738545623065874679014086723327636718749999999999999999999999999999999999999e-308", &TokenKind::RealLiteral, expect![[]]);
        expect_with_error("2.22507385850720113605740979670913197593481954635164564802342610972482222202107694551652952390813508791414915891303962110687008643869459464552765720740782062174337998814106326732925355228688137214901298112245145188984905722230728525513315575501591439747639798341180199932396254828901710708185069063066665599493827577257201576306269066333264756530000924588831643303777979186961204949739037782970490505108060994073026293712895895000358379996720725430436028407889577179615094551674824347103070260914462157228988025818254518032570701886087211312807951223342628836862232150377566662250398253433597456888442390026549819838548794829220689472168983109969836584681402285424333066033985088644580400103493397042756718644338377048603786162277173854562306587467901408672332763671875e-308", &TokenKind::RealLiteral, expect![[]]);
        expect_with_error("0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000222507385850720138309023271733240406421921598046233183055332741688720443481391819585428315901251102056406733973103581100515243416155346010885601238537771882113077799353200233047961014744258363607192156504694250373420837525080665061665815894872049117996859163964850063590877011830487479978088775374994945158045160505091539985658247081864511353793580499211598108576605199243335211435239014879569960959128889160299264151106346631339366347758651302937176204732563178148566435087212282863764204484681140761391147706280168985324411002416144742161856716615054015428508471675290190316132277889672970737312333408698898317506783884692609277397797285865965494109136909540613646756870239867831529068098461721092462539672851562500000000000000001", &TokenKind::RealLiteral, expect![[]]);
        expect_with_error("179769313486231580793728971405303415079934132710037826936173778980444968292764750946649017977587207096330286416692887910946555547851940402630657488671505820681908902000708383676273854845817711531764475730270069855571366959622842914819860834936475292719074168444365510704342711559699508093042880177904174497791.9999999999999999999999999999999999999999999999999999999999999999999999", &TokenKind::RealLiteral, expect![[]]);
        expect_with_error(
            "2.47032822920623272e-324",
            &TokenKind::RealLiteral,
            expect![[]],
        );
        expect_with_error("6.631236871469758276785396630275967243399099947355303144249971758736286630139265439618068200788048744105960420552601852889715006376325666595539603330361800519107591783233358492337208057849499360899425128640718856616503093444922854759159988160304439909868291973931426625698663157749836252274523485312442358651207051292453083278116143932569727918709786004497872322193856150225415211997283078496319412124640111777216148110752815101775295719811974338451936095907419622417538473679495148632480391435931767981122396703443803335529756003353209830071832230689201383015598792184172909927924176339315507402234836120730914783168400715462440053817592702766213559042115986763819482654128770595766806872783349146967171293949598850675682115696218943412532098591327667236328125E-316", &TokenKind::RealLiteral, expect![[]]);
        expect_with_error("3.237883913302901289588352412501532174863037669423108059901297049552301970670676565786835742587799557860615776559838283435514391084153169252689190564396459577394618038928365305143463955100356696665629202017331344031730044369360205258345803431471660032699580731300954848363975548690010751530018881758184174569652173110473696022749934638425380623369774736560008997404060967498028389191878963968575439222206416981462690113342524002724385941651051293552601421155333430225237291523843322331326138431477823591142408800030775170625915670728657003151953664260769822494937951845801530895238439819708403389937873241463484205608000027270531106827387907791444918534771598750162812548862768493201518991668028251730299953143924168545708663913273994694463908672332763671875E-319", &TokenKind::RealLiteral, expect![[]]);
        expect_with_error("6.953355807847677105972805215521891690222119817145950754416205607980030131549636688806115726399441880065386399864028691275539539414652831584795668560082999889551357784961446896042113198284213107935110217162654939802416034676213829409720583759540476786936413816541621287843248433202369209916612249676005573022703244799714622116542188837770376022371172079559125853382801396219552418839469770514904192657627060319372847562301074140442660237844114174497210955449896389180395827191602886654488182452409583981389442783377001505462015745017848754574668342161759496661766020028752888783387074850773192997102997936619876226688096314989645766000479009083731736585750335262099860150896718774401964796827166283225641992040747894382698751809812609536720628966577351093292236328125E-310", &TokenKind::RealLiteral, expect![[]]);
        expect_with_error("3.339068557571188581835713701280943911923401916998521771655656997328440314559615318168849149074662609099998113009465566426808170378434065722991659642619467706034884424989741080790766778456332168200464651593995817371782125010668346652995912233993254584461125868481633343674905074271064409763090708017856584019776878812425312008812326260363035474811532236853359905334625575404216060622858633280744301892470300555678734689978476870369853549413277156622170245846166991655321535529623870646888786637528995592800436177901746286272273374471701452991433047257863864601424252024791567368195056077320885329384322332391564645264143400798619665040608077549162173963649264049738362290606875883456826586710961041737908872035803481241600376705491726170293986797332763671875E-319", &TokenKind::RealLiteral, expect![[]]);
        expect_with_error("2.4703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772285886546332835517796989819938739800539093906315035659515570226392290858392449105184435931802849936536152500319370457678249219365623669863658480757001585769269903706311928279558551332927834338409351978015531246597263579574622766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968951340535537458516661134223766678604162159680461914467291840300530057530849048765391711386591646239524912623653881879636239373280423891018672348497668235089863388587925628302755995657524455507255189313690836254779186948667994968324049705821028513185451396213837722826145437693412532098591327667236328124999e-324", &TokenKind::RealLiteral, expect![[]]);
        expect_with_error("2.4703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772285886546332835517796989819938739800539093906315035659515570226392290858392449105184435931802849936536152500319370457678249219365623669863658480757001585769269903706311928279558551332927834338409351978015531246597263579574622766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968951340535537458516661134223766678604162159680461914467291840300530057530849048765391711386591646239524912623653881879636239373280423891018672348497668235089863388587925628302755995657524455507255189313690836254779186948667994968324049705821028513185451396213837722826145437693412532098591327667236328125e-324", &TokenKind::RealLiteral, expect![[]]);
        expect_with_error("2.4703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772285886546332835517796989819938739800539093906315035659515570226392290858392449105184435931802849936536152500319370457678249219365623669863658480757001585769269903706311928279558551332927834338409351978015531246597263579574622766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968951340535537458516661134223766678604162159680461914467291840300530057530849048765391711386591646239524912623653881879636239373280423891018672348497668235089863388587925628302755995657524455507255189313690836254779186948667994968324049705821028513185451396213837722826145437693412532098591327667236328125001e-324", &TokenKind::RealLiteral, expect![[]]);
        expect_with_error("7.4109846876186981626485318930233205854758970392148714663837852375101326090531312779794975454245398856969484704316857659638998506553390969459816219401617281718945106978546710679176872575177347315553307795408549809608457500958111373034747658096871009590975442271004757307809711118935784838675653998783503015228055934046593739791790738723868299395818481660169122019456499931289798411362062484498678713572180352209017023903285791732520220528974020802906854021606612375549983402671300035812486479041385743401875520901590172592547146296175134159774938718574737870961645638908718119841271673056017045493004705269590165763776884908267986972573366521765567941072508764337560846003984904972149117463085539556354188641513168478436313080237596295773983001708984374999e-324", &TokenKind::RealLiteral, expect![[]]);
        expect_with_error("7.4109846876186981626485318930233205854758970392148714663837852375101326090531312779794975454245398856969484704316857659638998506553390969459816219401617281718945106978546710679176872575177347315553307795408549809608457500958111373034747658096871009590975442271004757307809711118935784838675653998783503015228055934046593739791790738723868299395818481660169122019456499931289798411362062484498678713572180352209017023903285791732520220528974020802906854021606612375549983402671300035812486479041385743401875520901590172592547146296175134159774938718574737870961645638908718119841271673056017045493004705269590165763776884908267986972573366521765567941072508764337560846003984904972149117463085539556354188641513168478436313080237596295773983001708984375e-324", &TokenKind::RealLiteral, expect![[]]);
        expect_with_error("7.4109846876186981626485318930233205854758970392148714663837852375101326090531312779794975454245398856969484704316857659638998506553390969459816219401617281718945106978546710679176872575177347315553307795408549809608457500958111373034747658096871009590975442271004757307809711118935784838675653998783503015228055934046593739791790738723868299395818481660169122019456499931289798411362062484498678713572180352209017023903285791732520220528974020802906854021606612375549983402671300035812486479041385743401875520901590172592547146296175134159774938718574737870961645638908718119841271673056017045493004705269590165763776884908267986972573366521765567941072508764337560846003984904972149117463085539556354188641513168478436313080237596295773983001708984375001e-324", &TokenKind::RealLiteral, expect![[]]);
        expect_with_error(
            "94393431193180696942841837085033647913224148539854e-358",
            &TokenKind::RealLiteral,
            expect![[]],
        );
        expect_with_error("104308485241983990666713401708072175773165034278685682646111762292409330928739751702404658197872319129036519947435319418387839758990478549477777586673075945844895981012024387992135617064532141489278815239849108105951619997829153633535314849999674266169258928940692239684771590065027025835804863585454872499320500023126142553932654370362024104462255244034053203998964360882487378334860197725139151265590832887433736189468858614521708567646743455601905935595381852723723645799866672558576993978025033590728687206296379801363024094048327273913079612469982585674824156000783167963081616214710691759864332339239688734656548790656486646106983450809073750535624894296242072010195710276073042036425579852459556183541199012652571123898996574563824424330960027873516082763671875e-1075", &TokenKind::RealLiteral, expect![[]]);
    }

    #[test]
    fn scan_radix_ints() {
        expect("16#EABC", &TokenKind::RadixLiteral);

        // Errors
        // No errors for invalid literals are reported here

        // Overflow
        expect("10#99999999999999999999", &TokenKind::RadixLiteral);

        // No digits
        expect("30#", &TokenKind::RadixLiteral);
        expect_seq(
            "30#\n",
            &[
                (TokenKind::RadixLiteral, "30#"),
                (TokenKind::Whitespace, "\n"),
            ],
        );

        // Out of range (> 36)
        expect("37#asda", &TokenKind::RadixLiteral);

        // Out of range (= 0)
        expect("0#0000", &TokenKind::RadixLiteral);

        // Out of range (= 1)
        expect("1#0000", &TokenKind::RadixLiteral);

        // Out of range (= overflow)
        expect("18446744073709551616#0000", &TokenKind::RadixLiteral);

        // Invalid digit
        expect_seq(
            "    10#999a9a9",
            &[
                (TokenKind::Whitespace, "    "),
                (TokenKind::RadixLiteral, "10#999a9a9"),
            ],
        );
    }

    #[test]
    fn scan_basic_ints() {
        expect("01234560", &TokenKind::IntLiteral);

        // Overflow (No errors for invalid literals are reported here)
        expect("99999999999999999999", &TokenKind::IntLiteral);

        // Digit cutoff
        expect_seq(
            "999a999",
            &[
                (TokenKind::IntLiteral, "999"),
                (TokenKind::Identifier, "a999"),
            ],
        );
    }

    #[test]
    fn scan_literal_and_bits() {
        expect_seq(
            "..1",
            &[(TokenKind::Range, ".."), (TokenKind::IntLiteral, "1")],
        );
        expect_seq(
            "1..",
            &[(TokenKind::IntLiteral, "1"), (TokenKind::Range, "..")],
        );
        expect_seq(
            "1eggy",
            &[
                (TokenKind::RealLiteral, "1e"),
                (TokenKind::Identifier, "ggy"),
            ],
        );
    }

    #[test]
    fn scan_punct() {
        expect("@", &TokenKind::At);
        expect("&", &TokenKind::Ampersand);
        expect("->", &TokenKind::Arrow);
        expect("^", &TokenKind::Caret);
        expect(":", &TokenKind::Colon);
        expect(":=", &TokenKind::Assign);
        expect(",", &TokenKind::Comma);
        expect(".", &TokenKind::Dot);
        expect("..", &TokenKind::Range);
        expect("=", &TokenKind::Equ);
        expect(">=", &TokenKind::GreaterEqu);
        expect(">", &TokenKind::Greater);
        expect("#", &TokenKind::Pound);
        expect("=>", &TokenKind::Imply);
        expect("<=", &TokenKind::LessEqu);
        expect("(", &TokenKind::LeftParen);
        expect("<", &TokenKind::Less);
        expect("-", &TokenKind::Minus);
        expect("+", &TokenKind::Plus);
        expect("|", &TokenKind::Pipe);
        expect(")", &TokenKind::RightParen);
        expect(";", &TokenKind::Semicolon);
        expect("/", &TokenKind::Slash);
        expect("*", &TokenKind::Star);
        expect("**", &TokenKind::Exp);
        expect("~", &TokenKind::Tilde);
    }

    #[test]
    fn scan_keywords() {
        expect("addressint", &TokenKind::Addressint);
        expect("all", &TokenKind::All);
        expect("and", &TokenKind::And);
        expect("array", &TokenKind::Array);
        expect("asm", &TokenKind::Asm);
        expect("assert", &TokenKind::Assert);
        expect("begin", &TokenKind::Begin);
        expect("bind", &TokenKind::Bind);
        expect("bits", &TokenKind::Bits);
        expect("body", &TokenKind::Body);
        expect("boolean", &TokenKind::Boolean);
        expect("break", &TokenKind::Break);
        expect("by", &TokenKind::By);
        expect("case", &TokenKind::Case);
        expect("char", &TokenKind::Char);
        expect("cheat", &TokenKind::Cheat);
        expect("checked", &TokenKind::Checked);
        expect("class", &TokenKind::Class);
        expect("close", &TokenKind::Close);
        expect("collection", &TokenKind::Collection);
        expect("condition", &TokenKind::Condition);
        expect("const", &TokenKind::Const);
        expect("decreasing", &TokenKind::Decreasing);
        expect("def", &TokenKind::Def);
        expect("deferred", &TokenKind::Deferred);
        expect("div", &TokenKind::Div);
        expect("elif", &TokenKind::Elif);
        expect("else", &TokenKind::Else);
        expect("elseif", &TokenKind::Elseif);
        expect("elsif", &TokenKind::Elsif);
        expect("end", &TokenKind::End);
        expect("endcase", &TokenKind::EndCase);
        expect("endfor", &TokenKind::EndFor);
        expect("endif", &TokenKind::EndIf);
        expect("endloop", &TokenKind::EndLoop);
        expect("enum", &TokenKind::Enum);
        expect("exit", &TokenKind::Exit);
        expect("export", &TokenKind::Export);
        expect("external", &TokenKind::External);
        expect("false", &TokenKind::False);
        expect("flexible", &TokenKind::Flexible);
        expect("for", &TokenKind::For);
        expect("fork", &TokenKind::Fork);
        expect("forward", &TokenKind::Forward);
        expect("free", &TokenKind::Free);
        expect("function", &TokenKind::Function);
        expect("get", &TokenKind::Get);
        expect("handler", &TokenKind::Handler);
        expect("if", &TokenKind::If);
        expect("implement", &TokenKind::Implement);
        expect("import", &TokenKind::Import);
        expect("in", &TokenKind::In);
        expect("include", &TokenKind::Include);
        expect("inherit", &TokenKind::Inherit);
        expect("init", &TokenKind::Init);
        expect("int", &TokenKind::Int);
        expect("int1", &TokenKind::Int1);
        expect("int2", &TokenKind::Int2);
        expect("int4", &TokenKind::Int4);
        expect("invariant", &TokenKind::Invariant);
        expect("label", &TokenKind::Label);
        expect("loop", &TokenKind::Loop);
        expect("mod", &TokenKind::Mod);
        expect("module", &TokenKind::Module);
        expect("monitor", &TokenKind::Monitor);
        expect("nat", &TokenKind::Nat);
        expect("nat1", &TokenKind::Nat1);
        expect("nat2", &TokenKind::Nat2);
        expect("nat4", &TokenKind::Nat4);
        expect("new", &TokenKind::New);
        expect("nil", &TokenKind::Nil);
        expect("not", &TokenKind::Not);
        expect("objectclass", &TokenKind::ObjectClass);
        expect("of", &TokenKind::Of);
        expect("opaque", &TokenKind::Opaque);
        expect("open", &TokenKind::Open);
        expect("or", &TokenKind::Or);
        expect("packed", &TokenKind::Packed);
        expect("pause", &TokenKind::Pause);
        expect("pervasive", &TokenKind::Pervasive);
        expect("pointer", &TokenKind::Pointer);
        expect("post", &TokenKind::Post);
        expect("pre", &TokenKind::Pre);
        expect("priority", &TokenKind::Priority);
        expect("procedure", &TokenKind::Procedure);
        expect("process", &TokenKind::Process);
        expect("put", &TokenKind::Put);
        expect("quit", &TokenKind::Quit);
        expect("read", &TokenKind::Read);
        expect("real", &TokenKind::Real);
        expect("real4", &TokenKind::Real4);
        expect("real8", &TokenKind::Real8);
        expect("record", &TokenKind::Record);
        expect("register", &TokenKind::Register);
        expect("rem", &TokenKind::Rem);
        expect("result", &TokenKind::Result_);
        expect("return", &TokenKind::Return);
        expect("seek", &TokenKind::Seek);
        expect("self", &TokenKind::Self_);
        expect("set", &TokenKind::Set);
        expect("shl", &TokenKind::Shl);
        expect("shr", &TokenKind::Shr);
        expect("signal", &TokenKind::Signal);
        expect("skip", &TokenKind::Skip);
        expect("string", &TokenKind::String_);
        expect("tag", &TokenKind::Tag);
        expect("tell", &TokenKind::Tell);
        expect("then", &TokenKind::Then);
        expect("timeout", &TokenKind::Timeout);
        expect("to", &TokenKind::To);
        expect("true", &TokenKind::True);
        expect("type", &TokenKind::Type);
        expect("unchecked", &TokenKind::Unchecked);
        expect("union", &TokenKind::Union);
        expect("unit", &TokenKind::Unit);
        expect("unqualified", &TokenKind::Unqualified);
        expect("var", &TokenKind::Var);
        expect("wait", &TokenKind::Wait);
        expect("when", &TokenKind::When);
        expect("write", &TokenKind::Write);
        expect("xor", &TokenKind::Xor);
    }

    #[test]
    fn scan_identifiers() {
        expect("_source_text", &TokenKind::Identifier);
        // Skip over initial digits
        expect_seq(
            "0123_separate",
            &[
                (TokenKind::IntLiteral, "0123"),
                (TokenKind::Identifier, "_separate"),
            ],
        );
        // '.' is not part of an identifier, but "ba" & "e" should still be parsed
        expect_seq(
            "ba.e",
            &[
                (TokenKind::Identifier, "ba"),
                (TokenKind::Dot, "."),
                (TokenKind::Identifier, "e"),
            ],
        );
    }

    #[test]
    fn scan_line_comments() {
        expect_seq(
            "% abcd asd\n asd",
            &[
                (TokenKind::Comment, "% abcd asd"),
                (TokenKind::Whitespace, "\n "),
                (TokenKind::Identifier, "asd"),
            ],
        );

        expect("% abcd asd", &TokenKind::Comment);
    }

    #[test]
    fn scan_block_comments() {
        // Minimal block comment
        expect("/**/", &TokenKind::Comment);
        expect("/* */", &TokenKind::Comment);

        expect_seq(
            "/* /* abcd % * / \n\n\r\n */ */ asd",
            &[
                (TokenKind::Comment, "/* /* abcd % * / \n\n\r\n */ */"),
                (TokenKind::Whitespace, " "),
                (TokenKind::Identifier, "asd"),
            ],
        );

        // Sibling nodes
        expect("/* /* abcd */ /* ae */ */", &TokenKind::Comment);

        //// Missing terminating */ ////
        // Bare minimum with missing */
        expect_with_error(
            "/* ",
            &TokenKind::Comment,
            expect![[r#"error at 0..3: block comment is missing terminating ’*/’"#]],
        );

        // Respecting nesting
        expect_with_error(
            "/* /* abcd */",
            &TokenKind::Comment,
            expect![[r#"error at 0..13: block comment is missing terminating ’*/’"#]],
        );

        // With trailing spaces
        expect_with_error(
            "/* /* abcd */ ",
            &TokenKind::Comment,
            expect![[r#"error at 0..14: block comment is missing terminating ’*/’"#]],
        );
    }

    #[test]
    fn scan_string_literal() {
        // String literal parsing
        expect_seq(
            r#""abcd💖"a"#,
            &[
                (TokenKind::StringLiteral, r#""abcd💖""#),
                (TokenKind::Identifier, r#"a"#),
            ],
        );

        // not an escaped terminator
        expect(r#""abcd\\""#, &TokenKind::StringLiteral);
        expect(r#""abcd\\\k""#, &TokenKind::StringLiteral);
        expect(r#""abcd^^""#, &TokenKind::StringLiteral);
        expect(r#""abcd^^a""#, &TokenKind::StringLiteral);

        // Invalid scanning should allow a literal from the successfully parsed characters
        // Unterminated literals are not reported here, and is deferred to HIR lowering

        // Ends at the end of line
        expect_seq(
            "\"abcd\n",
            &[
                (TokenKind::StringLiteral, "\"abcd"),
                (TokenKind::Whitespace, "\n"),
            ],
        );

        expect_seq(
            "\"abcd\r\n",
            &[
                (TokenKind::StringLiteral, "\"abcd"),
                (TokenKind::Whitespace, "\r\n"),
            ],
        );

        // Ends at the end of file
        expect("\"abcd", &TokenKind::StringLiteral);

        // Escaped terminator
        expect(r#""abcd\""#, &TokenKind::StringLiteral);
        expect(r#""abcd^""#, &TokenKind::StringLiteral);

        // Mismatched delimiter
        expect("\"abcd'", &TokenKind::StringLiteral);

        // Empty
        expect(r#"""#, &TokenKind::StringLiteral);
    }

    #[test]
    fn scan_char_literal() {
        // Char(n) literal parsing
        expect_seq(
            "'abcd💖'a",
            &[
                (TokenKind::CharLiteral, "'abcd💖'"),
                (TokenKind::Identifier, "a"),
            ],
        );

        // not an escaped terminator
        expect(r#"'abcd\\'"#, &TokenKind::CharLiteral);

        // Invalid scanning should allow a literal from the successfully parsed characters
        // Unterminated literals are not reported here, and is deferred to HIR lowering

        // Ends at the end of line
        expect_seq(
            "'abcd\n",
            &[
                (TokenKind::CharLiteral, "'abcd"),
                (TokenKind::Whitespace, "\n"),
            ],
        );

        expect_seq(
            "'abcd\r\n",
            &[
                (TokenKind::CharLiteral, "'abcd"),
                (TokenKind::Whitespace, "\r\n"),
            ],
        );

        // Ends at the end of file
        expect("'abcd", &TokenKind::CharLiteral);

        // Escaped terminator
        expect(r#"'abcd\'"#, &TokenKind::CharLiteral);

        // Mismatched delimiter
        expect("'abcd\"", &TokenKind::CharLiteral);

        // Empty
        expect(r#"'"#, &TokenKind::CharLiteral);
    }

    #[test]
    fn token_aliases() {
        // Aliases
        expect("fcn", &TokenKind::Function);
        expect("proc", &TokenKind::Procedure);
    }

    // Picked up by the fuzzer

    #[test]
    fn block_comment_missing_endings() {
        expect_with_error(
            "/*/*",
            &TokenKind::Comment,
            expect![[r#"error at 0..4: block comment is missing terminating ’*/’"#]],
        );
    }

    #[test]
    fn radix_literal_dont_parse_non_ascii_chars() {
        expect_seq_with_errors(
            "2#پ",
            &[(TokenKind::RadixLiteral, "2#"), (TokenKind::Error, "پ")],
            expect![[r#"error at 2..4: invalid character"#]],
        );
    }
}
