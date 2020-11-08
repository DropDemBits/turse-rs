% Identifier is not a reference to a type
% TODO: Test dot references for records, unions, monitors, and modules once those are valid & resolvable

% Enum fields aren't type refs
type e : enum (c)
type ty0 : e.c
var v0 : e.c

% var/const refs aren't type refs
var vi : int := 1
type ty1 : vi
var v1 : vi := 2

const ci : int := 1
type ty2 : ci
var v2 : ci := 2

%%% args: -M -b
%%% expected exit status: 255

%%% expected stdout:

%%% expected stderr:
%%% error line:6 column:14-15 Field 'c' of 'e' does not refer to a type
%%% error line:7 column:12-13 Field 'c' of 'e' does not refer to a type
%%% error line:11 column:12-14 'vi' does not refer to a type
%%% error line:12 column:10-12 'vi' does not refer to a type
%%% error line:15 column:12-14 'ci' does not refer to a type
%%% error line:16 column:10-12 'ci' does not refer to a type