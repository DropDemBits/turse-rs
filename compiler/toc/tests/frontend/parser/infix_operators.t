% Test all operators in infix positions
% Should not crash

% Types don't matter here, that's checked in validator
const a := 1 + 1
const a := 1 - 1
const a := 1 * 1
const a := 1 div 1
const a := 1 shl 1
const a := 1 shr 1
const a := 1 and 1
const a := 1 or 1
const a := 1 & 1
const a := 1 | 1
const a := 1 xor 1
const a := 1 in 1
const a := 1 not in 1
const a := 1 ~ in 1
const a := 1 ~in 1
const a := 1 < 1
const a := 1 <= 1
const a := 1 > 1
const a := 1 >= 1
const a := 1 = 1
const a := 1 ~= 1
const a := 1 ~ = 1
const a := 1 not = 1
const a := 1 not= 1
const a := 1 => 1

const ba := 2
const a := ba.a
const a := ba->a
const a := ba()
const a := ba(1, 2, 3)

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% const [id:0] := nat(1) + nat(1)
%%% const [id:1] := nat(1) - nat(1)
%%% const [id:2] := nat(1) * nat(1)
%%% const [id:3] := nat(1) div nat(1)
%%% const [id:4] := nat(1) shl nat(1)
%%% const [id:5] := nat(1) shr nat(1)
%%% const [id:6] := nat(1) and nat(1)
%%% const [id:7] := nat(1) or nat(1)
%%% const [id:8] := nat(1) and nat(1)
%%% const [id:9] := nat(1) or nat(1)
%%% const [id:10] := nat(1) xor nat(1)
%%% const [id:11] := nat(1) in nat(1)
%%% const [id:12] := nat(1) not in nat(1)
%%% const [id:13] := nat(1) not in nat(1)
%%% const [id:14] := nat(1) not in nat(1)
%%% const [id:15] := nat(1) < nat(1)
%%% const [id:16] := nat(1) <= nat(1)
%%% const [id:17] := nat(1) > nat(1)
%%% const [id:18] := nat(1) >= nat(1)
%%% const [id:19] := nat(1) = nat(1)
%%% const [id:20] := nat(1) not= nat(1)
%%% const [id:21] := nat(1) not= nat(1)
%%% const [id:22] := nat(1) not= nat(1)
%%% const [id:23] := nat(1) not= nat(1)
%%% const [id:24] := nat(1) => nat(1)
%%% const [id:25] := nat(2)
%%% const [id:26] := ref(id:25) . a
%%% const [id:27] := ref(id:25) -> a
%%% const [id:28] := ref(id:25)()
%%% const [id:29] := ref(id:25)(nat(1), nat(2), nat(3))
%%% ]

%%% expected stderr:
