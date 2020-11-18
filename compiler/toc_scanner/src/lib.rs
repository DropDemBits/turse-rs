//! Scanner for lexing tokens

use logos::Logos;

/// All Tokens scanned by the Scanner
#[derive(Logos, Debug, PartialEq)]
pub enum ScannerToken {
    // Character Tokens
    #[token("@")]
    At,
    #[token("->")]
    Arrow,
    #[token("^")]
    Caret,
    #[token(":")]
    Colon,
    #[token(":=")]
    Assign,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("..")]
    Range,
    #[token("=")]
    Equ,
    #[token(">=")]
    GreaterEqu,
    #[token(">")]
    Greater,
    #[token("#")]
    Pound,
    #[token("=>")]
    Imply,
    #[token("<=")]
    LessEqu,
    #[token("(")]
    LeftParen,
    #[token("<")]
    Less,
    #[token("-")]
    Minus,
    #[token("+")]
    Plus,
    #[token(")")]
    RightParen,
    #[token(";")]
    Semicolon,
    #[token("/")]
    Slash,
    #[token("*")]
    Star,
    #[token("**")]
    Exp,
    #[token("~")]
    Tilde,

    // Keywords
    #[token("addressint")]
    Addressint,
    #[token("all")]
    All,
    #[token("and")]
    And,
    #[token("array")]
    Array,
    #[token("asm")]
    Asm,
    #[token("assert")]
    Assert,
    #[token("begin")]
    Begin,
    #[token("bind")]
    Bind,
    #[token("bits")]
    Bits,
    #[token("body")]
    Body,
    #[token("boolean")]
    Boolean,
    #[token("break")]
    Break,
    #[token("by")]
    By,
    #[token("case")]
    Case,
    #[token("char")]
    Char,
    #[token("cheat")]
    Cheat,
    #[token("checked")]
    Checked,
    #[token("class")]
    Class,
    #[token("close")]
    Close,
    #[token("collection")]
    Collection,
    #[token("condition")]
    Condition,
    #[token("const")]
    Const,
    #[token("decreasing")]
    Decreasing,
    #[token("def")]
    Def,
    #[token("deferred")]
    Deferred,
    #[token("div")]
    Div,
    #[token("elif")]
    Elif, // New keyword typo checking
    #[token("else")]
    Else,
    #[token("elseif")]
    Elseif,
    #[token("elsif")]
    Elsif,
    #[token("end")]
    End,
    #[token("endcase")]
    EndCase, // New keyword typo checking
    #[token("endfor")]
    EndFor,
    #[token("endif")]
    EndIf,
    #[token("endloop")]
    EndLoop,
    #[token("enum")]
    Enum,
    #[token("exit")]
    Exit,
    #[token("export")]
    Export,
    #[token("external")]
    External,
    #[token("false")]
    False,
    #[token("flexible")]
    Flexible,
    #[token("for")]
    For,
    #[token("fork")]
    Fork,
    #[token("forward")]
    Forward,
    #[token("free")]
    Free,
    #[token("fcn")]
    #[token("function")]
    Function,
    #[token("get")]
    Get,
    #[token("handler")]
    Handler,
    #[token("if")]
    If,
    #[token("implement")]
    Implement,
    #[token("import")]
    Import,
    #[token("in")]
    In,
    #[token("include")]
    Include,
    #[token("inherit")]
    Inherit,
    #[token("init")]
    Init,
    #[token("int")]
    Int,
    #[token("int1")]
    Int1,
    #[token("int2")]
    Int2,
    #[token("int4")]
    Int4,
    #[token("invariant")]
    Invariant,
    #[token("label")]
    Label,
    #[token("loop")]
    Loop,
    #[token("mod")]
    Mod,
    #[token("module")]
    Module,
    #[token("monitor")]
    Monitor,
    #[token("nat")]
    Nat,
    #[token("nat1")]
    Nat1,
    #[token("nat2")]
    Nat2,
    #[token("nat4")]
    Nat4,
    #[token("new")]
    New,
    #[token("nil")]
    Nil,
    #[token("not")]
    Not,
    #[token("objectclass")]
    ObjectClass,
    #[token("of")]
    Of,
    #[token("opaque")]
    Opaque,
    #[token("open")]
    Open,
    #[token("or")]
    Or,
    #[token("packed")]
    Packed,
    #[token("pause")]
    Pause,
    #[token("pervasive")]
    Pervasive,
    #[token("pointer")]
    Pointer,
    #[token("post")]
    Post,
    #[token("pre")]
    Pre,
    #[token("priority")]
    Priority,
    #[token("proc")]
    #[token("procedure")]
    Procedure,
    #[token("process")]
    Process,
    #[token("put")]
    Put,
    #[token("quit")]
    Quit,
    #[token("read")]
    Read,
    #[token("real")]
    Real,
    #[token("real4")]
    Real4,
    #[token("real8")]
    Real8,
    #[token("record")]
    Record,
    #[token("register")]
    Register,
    #[token("rem")]
    Rem,
    #[token("result")]
    Result_,
    #[token("return")]
    Return,
    #[token("seek")]
    Seek,
    #[token("self")]
    Self_,
    #[token("set")]
    Set,
    #[token("shl")]
    Shl,
    #[token("shr")]
    Shr,
    #[token("signal")]
    Signal,
    #[token("skip")]
    Skip,
    #[token("string")]
    String_,
    #[token("tag")]
    Tag,
    #[token("tell")]
    Tell,
    #[token("then")]
    Then,
    #[token("timeout")]
    Timeout,
    #[token("to")]
    To,
    #[token("true")]
    True,
    #[token("type")]
    Type,
    #[token("unchecked")]
    Unchecked,
    #[token("union")]
    Union,
    #[token("unit")]
    Unit,
    #[token("unqualified")]
    Unqualified,
    #[token("var")]
    Var,
    #[token("wait")]
    Wait,
    #[token("when")]
    When,
    #[token("write")]
    Write,
    #[token("xor")]
    Xor,

    // Literals
    #[regex("[[:alpha:]_][[:alpha:][:digit:]_]*")]
    Identifier,
    #[regex("'[^\r\n']*('?)")]
    CharLiteral,
    #[regex("\"[^\r\n\"]*(\"?)")]
    StringLiteral,
    /// Normal integer literal
    IntLiteral,
    /// Real Literals
    RealLiteral,

    /// All whitespace
    #[regex("[ \t]+")]
    Whitespace,

    /// All comments
    #[regex("%[^\r\n]*")]
    #[token("/*", lex_block_comment)]
    Comment,

    /// End of line
    #[regex("[\r\n]|(\r\n)")]
    LineEnd,

    #[error]
    Error,

    // All tokens below this point are never passed to the parser //

    // Entry point for other number literals
    #[doc(hidden)]
    #[regex("[[:digit:]]+", lex_number_literal)]
    #[regex("\\.[[:digit:]]+", lex_real_literal)]
    NumberLiteral(LiteralKind),
}

#[derive(Debug, PartialEq)]
pub enum LiteralKind {
    Int,
    Real,
}

fn lex_real_literal(lex: &mut logos::Lexer<ScannerToken>) -> Option<LiteralKind> {
    if lex.slice().ends_with('.') {
        // Bump the numbers
        let count = lex
            .remainder()
            .chars()
            .take_while(|c| matches!(c, '0'..='9'))
            .count();
        lex.bump(count);
    }

    if lex.remainder().starts_with(|c| matches!(c, 'e' | 'E')) {
        // bump e & E
        lex.bump(1);

        if lex.remainder().starts_with(|c| matches!(c, '+' | '-')) {
            lex.bump(1);
        }

        // Bump the rest of the exponent
        let count = lex
            .remainder()
            .chars()
            .take_while(|c| matches!(c, '0'..='9'))
            .count();
        lex.bump(count);

        if count == 0 {
            // Not a valid sequence
            return None;
        }
    }

    let text: &str = lex.slice();
    strtod::strtod(text).map(|_| LiteralKind::Real)
}

fn lex_number_literal(lex: &mut logos::Lexer<ScannerToken>) -> Option<LiteralKind> {
    // A few possible tails:
    // - `(\\.[0-9]+)?([eE][+-]?[0-9]+)`    as real literal
    // - `#[0-9a-zA-Z]*`                    as based
    // - .*`                                as int

    // Can't quite parse the corresponding values as `rowan` doesn't carry
    // that info over through SyntaxKinds

    // peekahead
    match lex.remainder().get(0..1) {
        Some(".") if !lex.remainder().starts_with("..") => {
            // Bump the dot
            lex.bump(1);
            lex_real_literal(lex)
        }
        Some("e") | Some("E") => lex_real_literal(lex),
        Some("#") => {
            // Bump the '#'
            lex.bump(1);
            // bump the rest
            let count = lex
                .remainder()
                .chars()
                .take_while(char::is_ascii_alphanumeric)
                .count();
            lex.bump(count);

            // Always give back something
            Some(LiteralKind::Int)
        }
        _ => {
            // normal, give back regular
            Some(LiteralKind::Int)
        }
    }
}

fn lex_block_comment(lex: &mut logos::Lexer<ScannerToken>) {
    // Continue to lex everything
    let mut bump_len = 0_usize;
    let mut comment_nesting = 1_usize;
    let remainder: &str = lex.remainder();

    // Track all the endings
    for in_between in remainder.split_terminator("*/") {
        // Adjust nesting
        comment_nesting = comment_nesting
            .saturating_add(in_between.matches("/*").count())
            .saturating_sub(1);
        // Increase bump length (include `*/`)
        bump_len = bump_len.saturating_add(in_between.len() + 2);

        if comment_nesting == 0 {
            // Outside of nesting depth, done
            // Cap to maximum length of remainder to prevent
            // an out of bounds bump
            lex.bump(bump_len.min(remainder.len()));
            return;
        }
    }

    // died
    lex.bump(bump_len);
}

/// Scanner for tokens
pub struct Scanner<'s> {
    inner: logos::Lexer<'s, ScannerToken>,
}

impl<'s> Scanner<'s> {
    pub fn new(source: &'s str) -> Self {
        Self {
            inner: ScannerToken::lexer(source),
        }
    }
}

impl<'s> std::iter::Iterator for Scanner<'s> {
    type Item = (ScannerToken, &'s str);

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next()?;
        let text = self.inner.slice();

        // Transform the token kind
        let kind = match kind {
            ScannerToken::NumberLiteral(literal) => match literal {
                LiteralKind::Real => ScannerToken::RealLiteral,
                LiteralKind::Int => ScannerToken::IntLiteral,
            },
            other => other,
        };

        Some((kind, text))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    /// Runs the lexer over the given text, and asserts if the expected token
    /// has been scanned
    #[track_caller] // Issue isn't here, but from caller
    fn expect(source: &str, kind: &ScannerToken) {
        let mut scanner = Scanner::new(source);

        // Should be the expected token & the same source text
        let text = scanner.next();
        assert_eq!(text.as_ref().map(|(tk, _)| tk), Some(kind));
        assert_eq!(text.as_ref().map(|(_, src)| *src), Some(source));
    }

    /// Runs the lexer over the given text, and asserts if the expected tokens
    /// have been scanned
    #[track_caller] // Issue isn't here, but from caller
    fn expect_seq(source: &str, kinds: &[(ScannerToken, &str)]) {
        let scanner = Scanner::new(source);

        // Should be the expected tokens & the same source texts
        let toks: Vec<(ScannerToken, &str)> = scanner.collect();
        assert_eq!(toks.is_empty(), kinds.is_empty());
        assert_eq!(toks, kinds);
    }

    #[test]
    fn scan_whitespace() {
        expect("     ", &ScannerToken::Whitespace);
        expect("     \t", &ScannerToken::Whitespace);
        expect("\n", &ScannerToken::LineEnd);
        expect("\r\n", &ScannerToken::LineEnd);
    }

    #[test]
    fn scan_invalid_chars() {
        // Lossless scanning
        expect("[", &ScannerToken::Error);
        expect("]", &ScannerToken::Error);
        expect("{", &ScannerToken::Error);
        expect("}", &ScannerToken::Error);
        expect("!", &ScannerToken::Error);
        expect("$", &ScannerToken::Error);
        expect("?", &ScannerToken::Error);
        expect("`", &ScannerToken::Error);
        expect("\\", &ScannerToken::Error);
        // Was originally `🧑‍🔬` but that gets split up into multiple tokens
        expect("🧑", &ScannerToken::Error);
    }

    #[test]
    #[ignore = "no normalization planned yet for identifiers"]
    fn text_normalization() {
        // Normalization issues: `â` and `â` are distinct
        // Text should be normalized, ie both should be characters
        expect("â", &ScannerToken::Error);
        expect("â", &ScannerToken::Error);
    }

    #[test]
    fn scan_real_literals() {
        // Allow for leading dot
        expect(".12345", &ScannerToken::RealLiteral);

        expect("1.", &ScannerToken::RealLiteral);
        expect("100.00", &ScannerToken::RealLiteral);
        expect("100.00e10", &ScannerToken::RealLiteral);
        expect("100.00e100", &ScannerToken::RealLiteral);

        // Negative and positive exponents are valid
        expect("100.00e-100", &ScannerToken::RealLiteral);
        expect("100.00e+100", &ScannerToken::RealLiteral);
        expect("1e100", &ScannerToken::RealLiteral);

        // Invalid format
        expect_seq("1e+", &[(ScannerToken::Error, "1e+")]);
        expect_seq("1e-", &[(ScannerToken::Error, "1e-")]);
        expect_seq("1e", &[(ScannerToken::Error, "1e")]);

        // Too big (not captured here?)
        expect("1e600", &ScannerToken::RealLiteral);

        // Test conversions
        let conversion_tests = &[
            "2.225073858507201136057409796709131975934819546351645648023426109724822222021076945516529523908135087914149158913039621106870086438694594645527657207407820621743379988141063267329253552286881372149012981122451451889849057222307285255133155755015914397476397983411801999323962548289017107081850690630666655994938275772572015763062690663332647565300009245888316433037779791869612049497390377829704905051080609940730262937128958950003583799967207254304360284078895771796150945516748243471030702609144621572289880258182545180325707018860872113128079512233426288368622321503775666622503982534335974568884423900265498198385487948292206894721689831099698365846814022854243330660339850886445804001034933970427567186443383770486037861622771738545623065874679014086723327636718749999999999999999999999999999999999999e-308",
            "2.22507385850720113605740979670913197593481954635164564802342610972482222202107694551652952390813508791414915891303962110687008643869459464552765720740782062174337998814106326732925355228688137214901298112245145188984905722230728525513315575501591439747639798341180199932396254828901710708185069063066665599493827577257201576306269066333264756530000924588831643303777979186961204949739037782970490505108060994073026293712895895000358379996720725430436028407889577179615094551674824347103070260914462157228988025818254518032570701886087211312807951223342628836862232150377566662250398253433597456888442390026549819838548794829220689472168983109969836584681402285424333066033985088644580400103493397042756718644338377048603786162277173854562306587467901408672332763671875e-308",
            "0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000222507385850720138309023271733240406421921598046233183055332741688720443481391819585428315901251102056406733973103581100515243416155346010885601238537771882113077799353200233047961014744258363607192156504694250373420837525080665061665815894872049117996859163964850063590877011830487479978088775374994945158045160505091539985658247081864511353793580499211598108576605199243335211435239014879569960959128889160299264151106346631339366347758651302937176204732563178148566435087212282863764204484681140761391147706280168985324411002416144742161856716615054015428508471675290190316132277889672970737312333408698898317506783884692609277397797285865965494109136909540613646756870239867831529068098461721092462539672851562500000000000000001",
            "179769313486231580793728971405303415079934132710037826936173778980444968292764750946649017977587207096330286416692887910946555547851940402630657488671505820681908902000708383676273854845817711531764475730270069855571366959622842914819860834936475292719074168444365510704342711559699508093042880177904174497791.9999999999999999999999999999999999999999999999999999999999999999999999",
            "2.47032822920623272e-324",
            "6.631236871469758276785396630275967243399099947355303144249971758736286630139265439618068200788048744105960420552601852889715006376325666595539603330361800519107591783233358492337208057849499360899425128640718856616503093444922854759159988160304439909868291973931426625698663157749836252274523485312442358651207051292453083278116143932569727918709786004497872322193856150225415211997283078496319412124640111777216148110752815101775295719811974338451936095907419622417538473679495148632480391435931767981122396703443803335529756003353209830071832230689201383015598792184172909927924176339315507402234836120730914783168400715462440053817592702766213559042115986763819482654128770595766806872783349146967171293949598850675682115696218943412532098591327667236328125E-316",
            "3.237883913302901289588352412501532174863037669423108059901297049552301970670676565786835742587799557860615776559838283435514391084153169252689190564396459577394618038928365305143463955100356696665629202017331344031730044369360205258345803431471660032699580731300954848363975548690010751530018881758184174569652173110473696022749934638425380623369774736560008997404060967498028389191878963968575439222206416981462690113342524002724385941651051293552601421155333430225237291523843322331326138431477823591142408800030775170625915670728657003151953664260769822494937951845801530895238439819708403389937873241463484205608000027270531106827387907791444918534771598750162812548862768493201518991668028251730299953143924168545708663913273994694463908672332763671875E-319",
            "6.953355807847677105972805215521891690222119817145950754416205607980030131549636688806115726399441880065386399864028691275539539414652831584795668560082999889551357784961446896042113198284213107935110217162654939802416034676213829409720583759540476786936413816541621287843248433202369209916612249676005573022703244799714622116542188837770376022371172079559125853382801396219552418839469770514904192657627060319372847562301074140442660237844114174497210955449896389180395827191602886654488182452409583981389442783377001505462015745017848754574668342161759496661766020028752888783387074850773192997102997936619876226688096314989645766000479009083731736585750335262099860150896718774401964796827166283225641992040747894382698751809812609536720628966577351093292236328125E-310",
            "3.339068557571188581835713701280943911923401916998521771655656997328440314559615318168849149074662609099998113009465566426808170378434065722991659642619467706034884424989741080790766778456332168200464651593995817371782125010668346652995912233993254584461125868481633343674905074271064409763090708017856584019776878812425312008812326260363035474811532236853359905334625575404216060622858633280744301892470300555678734689978476870369853549413277156622170245846166991655321535529623870646888786637528995592800436177901746286272273374471701452991433047257863864601424252024791567368195056077320885329384322332391564645264143400798619665040608077549162173963649264049738362290606875883456826586710961041737908872035803481241600376705491726170293986797332763671875E-319",
            "2.4703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772285886546332835517796989819938739800539093906315035659515570226392290858392449105184435931802849936536152500319370457678249219365623669863658480757001585769269903706311928279558551332927834338409351978015531246597263579574622766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968951340535537458516661134223766678604162159680461914467291840300530057530849048765391711386591646239524912623653881879636239373280423891018672348497668235089863388587925628302755995657524455507255189313690836254779186948667994968324049705821028513185451396213837722826145437693412532098591327667236328124999e-324",
            "2.4703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772285886546332835517796989819938739800539093906315035659515570226392290858392449105184435931802849936536152500319370457678249219365623669863658480757001585769269903706311928279558551332927834338409351978015531246597263579574622766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968951340535537458516661134223766678604162159680461914467291840300530057530849048765391711386591646239524912623653881879636239373280423891018672348497668235089863388587925628302755995657524455507255189313690836254779186948667994968324049705821028513185451396213837722826145437693412532098591327667236328125e-324",
            "2.4703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772285886546332835517796989819938739800539093906315035659515570226392290858392449105184435931802849936536152500319370457678249219365623669863658480757001585769269903706311928279558551332927834338409351978015531246597263579574622766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968951340535537458516661134223766678604162159680461914467291840300530057530849048765391711386591646239524912623653881879636239373280423891018672348497668235089863388587925628302755995657524455507255189313690836254779186948667994968324049705821028513185451396213837722826145437693412532098591327667236328125001e-324",
            "7.4109846876186981626485318930233205854758970392148714663837852375101326090531312779794975454245398856969484704316857659638998506553390969459816219401617281718945106978546710679176872575177347315553307795408549809608457500958111373034747658096871009590975442271004757307809711118935784838675653998783503015228055934046593739791790738723868299395818481660169122019456499931289798411362062484498678713572180352209017023903285791732520220528974020802906854021606612375549983402671300035812486479041385743401875520901590172592547146296175134159774938718574737870961645638908718119841271673056017045493004705269590165763776884908267986972573366521765567941072508764337560846003984904972149117463085539556354188641513168478436313080237596295773983001708984374999e-324",
            "7.4109846876186981626485318930233205854758970392148714663837852375101326090531312779794975454245398856969484704316857659638998506553390969459816219401617281718945106978546710679176872575177347315553307795408549809608457500958111373034747658096871009590975442271004757307809711118935784838675653998783503015228055934046593739791790738723868299395818481660169122019456499931289798411362062484498678713572180352209017023903285791732520220528974020802906854021606612375549983402671300035812486479041385743401875520901590172592547146296175134159774938718574737870961645638908718119841271673056017045493004705269590165763776884908267986972573366521765567941072508764337560846003984904972149117463085539556354188641513168478436313080237596295773983001708984375e-324",
            "7.4109846876186981626485318930233205854758970392148714663837852375101326090531312779794975454245398856969484704316857659638998506553390969459816219401617281718945106978546710679176872575177347315553307795408549809608457500958111373034747658096871009590975442271004757307809711118935784838675653998783503015228055934046593739791790738723868299395818481660169122019456499931289798411362062484498678713572180352209017023903285791732520220528974020802906854021606612375549983402671300035812486479041385743401875520901590172592547146296175134159774938718574737870961645638908718119841271673056017045493004705269590165763776884908267986972573366521765567941072508764337560846003984904972149117463085539556354188641513168478436313080237596295773983001708984375001e-324",
            "94393431193180696942841837085033647913224148539854e-358",
            "104308485241983990666713401708072175773165034278685682646111762292409330928739751702404658197872319129036519947435319418387839758990478549477777586673075945844895981012024387992135617064532141489278815239849108105951619997829153633535314849999674266169258928940692239684771590065027025835804863585454872499320500023126142553932654370362024104462255244034053203998964360882487378334860197725139151265590832887433736189468858614521708567646743455601905935595381852723723645799866672558576993978025033590728687206296379801363024094048327273913079612469982585674824156000783167963081616214710691759864332339239688734656548790656486646106983450809073750535624894296242072010195710276073042036425579852459556183541199012652571123898996574563824424330960027873516082763671875e-1075",
        ];

        for test in conversion_tests {
            expect(test, &ScannerToken::RealLiteral);
        }
    }

    #[test]
    fn scan_radix_ints() {
        expect("16#EABC", &ScannerToken::IntLiteral);

        // Overflow
        expect("10#99999999999999999999", &ScannerToken::IntLiteral);

        // No digits
        expect("30#", &ScannerToken::IntLiteral);

        // Out of range (> 36)
        expect("37#asda", &ScannerToken::IntLiteral);

        // Out of range (= 0)
        expect("0#0000", &ScannerToken::IntLiteral);

        // Out of range (= 1)
        expect("1#0000", &ScannerToken::IntLiteral);

        // Out of range (= overflow)
        expect("18446744073709551616#0000", &ScannerToken::IntLiteral);

        // Invalid digit
        expect("10#999a999", &ScannerToken::IntLiteral);
    }

    #[test]
    fn scan_basic_ints() {
        expect("01234560", &ScannerToken::IntLiteral);

        // Overflow, not detected
        expect("99999999999999999999", &ScannerToken::IntLiteral);

        // Digit cutoff
        expect_seq(
            "999a999",
            &[
                (ScannerToken::IntLiteral, "999"),
                (ScannerToken::Identifier, "a999"),
            ],
        );
    }

    #[test]
    fn scan_literal_and_range() {
        expect_seq(
            "..1",
            &[(ScannerToken::Range, ".."), (ScannerToken::IntLiteral, "1")],
        );
        expect_seq(
            "1..",
            &[(ScannerToken::IntLiteral, "1"), (ScannerToken::Range, "..")],
        );
        expect_seq(
            "1eggy",
            &[
                (ScannerToken::Error, "1e"),
                (ScannerToken::Identifier, "ggy"),
            ],
        );
    }

    #[test]
    fn scan_punct() {
        expect("@", &ScannerToken::At);
        expect("->", &ScannerToken::Arrow);
        expect("^", &ScannerToken::Caret);
        expect(":", &ScannerToken::Colon);
        expect(":=", &ScannerToken::Assign);
        expect(",", &ScannerToken::Comma);
        expect(".", &ScannerToken::Dot);
        expect("..", &ScannerToken::Range);
        expect("=", &ScannerToken::Equ);
        expect(">=", &ScannerToken::GreaterEqu);
        expect(">", &ScannerToken::Greater);
        expect("#", &ScannerToken::Pound);
        expect("=>", &ScannerToken::Imply);
        expect("<=", &ScannerToken::LessEqu);
        expect("(", &ScannerToken::LeftParen);
        expect("<", &ScannerToken::Less);
        expect("-", &ScannerToken::Minus);
        expect("+", &ScannerToken::Plus);
        expect(")", &ScannerToken::RightParen);
        expect(";", &ScannerToken::Semicolon);
        expect("/", &ScannerToken::Slash);
        expect("*", &ScannerToken::Star);
        expect("**", &ScannerToken::Exp);
        expect("~", &ScannerToken::Tilde);
    }

    #[test]
    fn scan_keywords() {
        expect("addressint", &ScannerToken::Addressint);
        expect("all", &ScannerToken::All);
        expect("and", &ScannerToken::And);
        expect("array", &ScannerToken::Array);
        expect("asm", &ScannerToken::Asm);
        expect("assert", &ScannerToken::Assert);
        expect("begin", &ScannerToken::Begin);
        expect("bind", &ScannerToken::Bind);
        expect("bits", &ScannerToken::Bits);
        expect("body", &ScannerToken::Body);
        expect("boolean", &ScannerToken::Boolean);
        expect("break", &ScannerToken::Break);
        expect("by", &ScannerToken::By);
        expect("case", &ScannerToken::Case);
        expect("char", &ScannerToken::Char);
        expect("cheat", &ScannerToken::Cheat);
        expect("checked", &ScannerToken::Checked);
        expect("class", &ScannerToken::Class);
        expect("close", &ScannerToken::Close);
        expect("collection", &ScannerToken::Collection);
        expect("condition", &ScannerToken::Condition);
        expect("const", &ScannerToken::Const);
        expect("decreasing", &ScannerToken::Decreasing);
        expect("def", &ScannerToken::Def);
        expect("deferred", &ScannerToken::Deferred);
        expect("div", &ScannerToken::Div);
        expect("elif", &ScannerToken::Elif);
        expect("else", &ScannerToken::Else);
        expect("elseif", &ScannerToken::Elseif);
        expect("elsif", &ScannerToken::Elsif);
        expect("end", &ScannerToken::End);
        expect("endcase", &ScannerToken::EndCase);
        expect("endfor", &ScannerToken::EndFor);
        expect("endif", &ScannerToken::EndIf);
        expect("endloop", &ScannerToken::EndLoop);
        expect("enum", &ScannerToken::Enum);
        expect("exit", &ScannerToken::Exit);
        expect("export", &ScannerToken::Export);
        expect("external", &ScannerToken::External);
        expect("false", &ScannerToken::False);
        expect("flexible", &ScannerToken::Flexible);
        expect("for", &ScannerToken::For);
        expect("fork", &ScannerToken::Fork);
        expect("forward", &ScannerToken::Forward);
        expect("free", &ScannerToken::Free);
        expect("function", &ScannerToken::Function);
        expect("get", &ScannerToken::Get);
        expect("handler", &ScannerToken::Handler);
        expect("if", &ScannerToken::If);
        expect("implement", &ScannerToken::Implement);
        expect("import", &ScannerToken::Import);
        expect("in", &ScannerToken::In);
        expect("include", &ScannerToken::Include);
        expect("inherit", &ScannerToken::Inherit);
        expect("init", &ScannerToken::Init);
        expect("int", &ScannerToken::Int);
        expect("int1", &ScannerToken::Int1);
        expect("int2", &ScannerToken::Int2);
        expect("int4", &ScannerToken::Int4);
        expect("invariant", &ScannerToken::Invariant);
        expect("label", &ScannerToken::Label);
        expect("loop", &ScannerToken::Loop);
        expect("mod", &ScannerToken::Mod);
        expect("module", &ScannerToken::Module);
        expect("monitor", &ScannerToken::Monitor);
        expect("nat", &ScannerToken::Nat);
        expect("nat1", &ScannerToken::Nat1);
        expect("nat2", &ScannerToken::Nat2);
        expect("nat4", &ScannerToken::Nat4);
        expect("new", &ScannerToken::New);
        expect("nil", &ScannerToken::Nil);
        expect("not", &ScannerToken::Not);
        expect("objectclass", &ScannerToken::ObjectClass);
        expect("of", &ScannerToken::Of);
        expect("opaque", &ScannerToken::Opaque);
        expect("open", &ScannerToken::Open);
        expect("or", &ScannerToken::Or);
        expect("packed", &ScannerToken::Packed);
        expect("pause", &ScannerToken::Pause);
        expect("pervasive", &ScannerToken::Pervasive);
        expect("pointer", &ScannerToken::Pointer);
        expect("post", &ScannerToken::Post);
        expect("pre", &ScannerToken::Pre);
        expect("priority", &ScannerToken::Priority);
        expect("procedure", &ScannerToken::Procedure);
        expect("process", &ScannerToken::Process);
        expect("put", &ScannerToken::Put);
        expect("quit", &ScannerToken::Quit);
        expect("read", &ScannerToken::Read);
        expect("real", &ScannerToken::Real);
        expect("real4", &ScannerToken::Real4);
        expect("real8", &ScannerToken::Real8);
        expect("record", &ScannerToken::Record);
        expect("register", &ScannerToken::Register);
        expect("rem", &ScannerToken::Rem);
        expect("result", &ScannerToken::Result_);
        expect("return", &ScannerToken::Return);
        expect("seek", &ScannerToken::Seek);
        expect("self", &ScannerToken::Self_);
        expect("set", &ScannerToken::Set);
        expect("shl", &ScannerToken::Shl);
        expect("shr", &ScannerToken::Shr);
        expect("signal", &ScannerToken::Signal);
        expect("skip", &ScannerToken::Skip);
        expect("string", &ScannerToken::String_);
        expect("tag", &ScannerToken::Tag);
        expect("tell", &ScannerToken::Tell);
        expect("then", &ScannerToken::Then);
        expect("timeout", &ScannerToken::Timeout);
        expect("to", &ScannerToken::To);
        expect("true", &ScannerToken::True);
        expect("type", &ScannerToken::Type);
        expect("unchecked", &ScannerToken::Unchecked);
        expect("union", &ScannerToken::Union);
        expect("unit", &ScannerToken::Unit);
        expect("unqualified", &ScannerToken::Unqualified);
        expect("var", &ScannerToken::Var);
        expect("wait", &ScannerToken::Wait);
        expect("when", &ScannerToken::When);
        expect("write", &ScannerToken::Write);
        expect("xor", &ScannerToken::Xor);
    }

    #[test]
    fn scan_identifiers() {
        expect("_source_text", &ScannerToken::Identifier);
        // Skip over initial digits
        expect_seq(
            "0123_separate",
            &[
                (ScannerToken::IntLiteral, "0123"),
                (ScannerToken::Identifier, "_separate"),
            ],
        );
        // Invalid character, but "ba" & "e" should still be parsed
        expect_seq(
            "ba$e",
            &[
                (ScannerToken::Identifier, "ba"),
                (ScannerToken::Error, "$"),
                (ScannerToken::Identifier, "e"),
            ],
        );
    }

    #[test]
    fn scan_line_comments() {
        expect_seq(
            "% abcd asd\n asd",
            &[
                (ScannerToken::Comment, "% abcd asd"),
                (ScannerToken::LineEnd, "\n"),
                (ScannerToken::Whitespace, " "),
                (ScannerToken::Identifier, "asd"),
            ],
        );

        expect("% abcd asd", &ScannerToken::Comment);
    }

    #[test]
    fn scan_block_comments() {
        expect_seq(
            "/* /* abcd % * / \n\n\r\n */ */ asd",
            &[
                (ScannerToken::Comment, "/* /* abcd % * / \n\n\r\n */ */"),
                (ScannerToken::Whitespace, " "),
                (ScannerToken::Identifier, "asd"),
            ],
        );

        // Sibling nodes
        expect("/* /* abcd */ /* ae */ */", &ScannerToken::Comment);

        // Missing terminating */
        expect("/* /* abcd */", &ScannerToken::Comment);
        expect("/* /* abcd */ ", &ScannerToken::Comment);
    }

    #[test]
    fn scan_string_literal() {
        // String literal parsing
        expect_seq(
            r#""abcd💖"a"#,
            &[
                (ScannerToken::StringLiteral, r#""abcd💖""#),
                (ScannerToken::Identifier, r#"a"#),
            ],
        );

        // Invalid scanning should make a literal from the successfully parsed character

        // Ends at the end of line
        expect_seq(
            "\"abcd\n",
            &[
                (ScannerToken::StringLiteral, "\"abcd"),
                (ScannerToken::LineEnd, "\n"),
            ],
        );

        expect_seq(
            "\"abcd\r\n",
            &[
                (ScannerToken::StringLiteral, "\"abcd"),
                (ScannerToken::LineEnd, "\r\n"),
            ],
        );

        // Ends at the end of file
        expect("\"abcd", &ScannerToken::StringLiteral);

        // Mismatched delimiter
        expect("\"abcd'", &ScannerToken::StringLiteral);
    }

    #[test]
    fn scan_char_literal() {
        // Char(n) literal parsing
        expect_seq(
            "'abcd💖'a",
            &[
                (ScannerToken::CharLiteral, "'abcd💖'"),
                (ScannerToken::Identifier, "a"),
            ],
        );

        // Invalid scanning should make a literal from the successfully parsed characters

        // Ends at the end of line
        expect_seq(
            "'abcd\n",
            &[
                (ScannerToken::CharLiteral, "'abcd"),
                (ScannerToken::LineEnd, "\n"),
            ],
        );

        expect_seq(
            "'abcd\r\n",
            &[
                (ScannerToken::CharLiteral, "'abcd"),
                (ScannerToken::LineEnd, "\r\n"),
            ],
        );

        // Ends at the end of file
        expect("'abcd", &ScannerToken::CharLiteral);

        // Mismatched delimiter
        expect("'abcd\"", &ScannerToken::CharLiteral);
    }

    #[test]
    fn token_aliases() {
        // Aliases
        expect("fcn", &ScannerToken::Function);
        expect("proc", &ScannerToken::Procedure);
    }
}
