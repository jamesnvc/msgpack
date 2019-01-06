:- module(msgpack, [msgpack//1]).
/** <module> Prolog MessagePack library

This module contains DCGs for packing & unpacking MessagePack data.

@author James Cash
@license GPLv3

@tbd double-precision floats

@bug Can't write out floats (can unpack single-precision, but not write)
*/
:- use_module(library(clpfd)).


%! msgpack(+MsgPack, -Bytes, ?_) is semidet.
%! msgpack(-MsgPack, +Bytes, ?_) is semidet.
% DCG for packing/unpacking MessagePack to/from a list of bytes.
%
% @see https://github.com/msgpack/msgpack/blob/master/spec.md
msgpack(none) --> nil, !.
msgpack(str(S)) --> str(str(S)), !.
msgpack(list(L)) --> array(list(L)), !.
msgpack(dict(D)) --> map(dict(D)), !.
msgpack(bin(X)) --> bin(bin(X)), !.
msgpack(date(Y,M,D,H,Mn,S,Off,TZ,DST)) -->
    { Dt = date(Y,M,D,H,Mn,S,Off,TZ,DST) }, timestamp(dt(Dt)), !.
msgpack(ext(T, X)) --> ext(ext(T, X)), !.
msgpack(single(N)) --> floating(single(N)), !.
msgpack(B) --> bool(B), !.
msgpack(N) --> int(N), !.

nil --> [0xc0].

bool(false)--> [0xc2].
bool(true) --> [0xc3].

% Integer types
int(N) --> fixnum(N).
int(N) --> uint8(N).
int(N) --> uint16(N).
int(N) --> uint32(N).
int(N) --> uint64(N).
int(N) --> int8(N).
int(N) --> int16(N).
int(N) --> int32(N).
int(N) --> int64(N).

% positive fixnum stores 7-bit positive integer
fixnum(N) -->
    [N],
    { N =< 0b0111 1111, N >= 0, ! }.
% negative fixnum stores 5-bit negative integer
fixnum(N) -->
    [X],
    { X in 224..255,
      N #< 0,
      N #>= -0b00011111,
      X #= 0b11100000 \/ V,
      V in 0..31,
      Inv #= 0b11111 - V,
      N #= -Inv - 1 }.
% uint8 stores an 8-bit unsigned integer
uint8(N) -->
    [0xcc, N],
    { N in 0..255 }.
%uint16 stores a 16-bit big-endian unsigned integer
uint16(N) -->
    { integer(N), N >= 0, N < 1<<17, ! },
    [0xcd, A, B],
    { B is N /\ 0xff,
      A is (N /\ 0xff00) >> 8 }.
uint16(N) -->
    [0xcd, A, B],
    { N #> 0, N #< 1 << 17,
      N is A<<8 + B }.
% uint32 stores a 32-bit big-endian unsigned integer
uint32(N) -->
    % special-case for when given an integer; we can be much faster
    % about how we pack it than the brute-force search that clp(fd)
    % would take
    { integer(N), N >= 1>>17, N < 1<<33, ! },
    [0xce, A, B, C, D],
    { D is N /\ 0xff,
      C is (N /\ 0xff00) >> 8,
      B is (N /\ 0xff0000) >> 16,
      A is (N /\ 0xff000000) >> 24 }.
uint32(N) -->
    [0xce, A, B, C, D],
    { N #> 0, N #< 1 << 33,
      N is D + C << 8 + B << 16 + A << 24 }.
% uint64 stores a 64-bit big-endian unsigned integer
uint64(N) -->
    % special-case for when given an integer; we can be much faster
    % about how we pack it than the brute-force search that clp(fd)
    % would take
    { integer(N), N >= 1>>33, N < 1<<65, ! },
    [0xcf, A, B, C, D, E, F, G, H],
    { H is N /\ 0xff,
      G is (N /\ 0xff00) >> 8,
      F is (N /\ 0xff0000) >> 16,
      E is (N /\ 0xff000000) >> 24,
      D is (N /\ 0xff00000000) >> 32,
      C is (N /\ 0xff0000000000) >> 40,
      B is (N /\ 0xff000000000000) >> 48,
      A is (N /\ 0xff00000000000000) >> 56 }.
uint64(N) -->
    [0xcf, A, B, C, D, E, F, G, H],
    { N #> 0, N #< 1 << 65,
      N is H + G<<8 + F<<16 + E<<24 + D<<32 + C<<40 + B<<48 + A<<56 }.
% int8 stores an 8-bit signed integer
% argument bytes are always unsigned, so need to convert
% NB. 0x80 = 0b1000 0000
int8(N) --> % neg int8
    [0xd0, A],
    { N in (-128)..(-1),
      A in 0..255,
      A #>= 0x80,
      Inv #= 0xff - A,
      N #= -Inv - 1 }.
int8(N) --> % pos int8
    [0xd0, N],
    { N in 0..127 }.
% int16
int16(N) -->
    { integer(N), N =< 0x7fff, N >= -0x8000 },
    [0xd1, A, B],
    { unsigned16_signed16(X, N),
      A is (X /\ 0xff00) >> 8,
      B is (X /\ 0x00ff) }.
int16(N) --> % neg int16
    [0xd1, A, B],
    { A #>= 0x80,
      N in (-0x8000)..(-1),
      X is A<<8 + B,
      Inv is 0xffff - X,
      N is -Inv - 1 }.
int16(N) --> % pos int16
    [0xd1, A, B],
    { N in 0..0x7fff,
      N is A<<8 + B }.
% int32
int32(N) -->
    { integer(N), N >= -0x8000_0000, N < 0x8000_0000, ! },
    [0xd2, A, B, C, D],
    { unsigned32_signed32(X, N),
      D is X /\ 0xff,
      C is (X /\ 0xff00) >> 8,
      B is (X /\ 0xff0000) >> 16,
      A is (X /\ 0xff000000) >> 24 }.
int32(N) --> % neg int32
    [0xd2, A, B, C, D],
    { N in (-0x8000_0000)..(-1),
      A #>= 0x80,
      X is A<<24 + B<<16 + C<<8 + D,
      Inv is 0xffff_ffff - X,
      N is -Inv - 1 }.
int32(N) --> % pos int32
    [0xd2, A, B, C, D],
    { N in 0..(0x7fff_ffff),
      N is A<<24 + B<<16 + C<<8 + D }.
% int64
int64(N) -->
    { integer(N), ! },
    [0xd3, A, B, C, D, E, F, G, H],
    { unsigned64_signed64(X, N),
      H is X /\ 0xff,
      G is (X /\ 0xff00) >> 8,
      F is (X /\ 0xff0000) >> 16,
      E is (X /\ 0xff000000) >> 24,
      D is (X /\ 0xff00000000) >> 32,
      C is (X /\ 0xff0000000000) >> 40,
      B is (X /\ 0xff000000000000) >> 48,
      A is (X /\ 0xff00000000000000) >> 56 }.
int64(N) --> % neg int64
    { N in (-0x8000_0000_0000_0000)..(-1) },
    [0xd3, A, B, C, D, E, F, G, H],
    { [A,B,C,D,E,F,G,H] ins 0..255,
      A #>= 0x80,
      X is A<<56 + B<<48 + C<<40 + D<<32 + E<<24 + F<<16 + G<<8 + H,
      Inv is 0xffff_ffff_ffff_ffff - X,
      N is -Inv - 1 }.
int64(N) --> % pos int64
    [0xd3, A, B, C, D, E, F, G, H],
    { [A,B,C,D,E,F,G,H] ins 0..255,
      N in 0..(0x7fff_ffff_ffff_ffff),
      N is A<<56 + B<<48 + C<<40 + D<<32 + E<<24 + F<<16 + G<<8 + H }.

% Floats
% TODO: use clp(r) for this?
float_bits(_,  N, -1,  _,   N) :- !.
float_bits(Bs, N, Bit, Div, Ans) :-
    Nn is N + (getbit(Bs, Bit) * Div),
    DivN is Div / 2,
    BitN is Bit - 1,
    float_bits(Bs, Nn, BitN, DivN, Ans).
float_bits(Bs, N) :- float_bits(Bs, 1, 22, 0.5, N).

floating(single(Fl)) -->
    [0xca, A, B, C, D],
    { [A,B,C,D] ins 0..255,
      Sign is (-1)**((A /\ 0b1000_0000) >> 7),
      Exp_ is (A /\ 0b0111_1111) << 1 + (B /\ 0b1000_0000) >> 7,
      Exp is 2**(Exp_ - 127),
      FracBits is (B /\ 0b0111_1111)<<16 + C<<8 + D,
      float_bits(FracBits, Frac),
      Fl is Sign * Exp * Frac }.
% TODO: gurf
% float(double(N)) -->
%     [0xcb, A, B, C, D, E, F, G, H],
%     { [A,B,C,D,E,F,G,H] ins 0..255 }.

% Strings

% string helper predicates
str_header(N, 0xd9) :- N < 1<<8.
str_header(N, 0xda) :- N < 1<<16.
str_header(N, 0xdb) :- N < 1<<32.

string_pad_bytes([B], [B]).
string_pad_bytes([B1, B2], [B1, B2]).
string_pad_bytes([B1, B2, B3], [0, B1, B2, B3]).
string_pad_bytes([B1, B2, B3, B4], [B1, B2, B3, B4]).

msgpack_string_len(S, Len) -->
    msgpack_string_len_(Len, Codes),
    { string_codes(S, Codes) }.
msgpack_string_len_(0, [],  L, L) :- !.
msgpack_string_len_(Len, [C|Codes]) -->
   [C],
   { Len1 #= Len - 1 },
   msgpack_string_len_(Len1, Codes).

str(str(S)) -->
    { string(S), string_length(S, L), L =< 31, ! },
    [H],
    { H is 0b10100000 \/ L,
      string_codes(S, Bytes) },
    Bytes.
str(str(S)) -->
    { string(S), string_length(S, L), L > 31, L < 1<<32, !,
      str_header(L, H),
      int_bytes(L, LenBytes_),
      string_pad_bytes(LenBytes_, LenBytes),
      !,
      string_codes(S, Bytes) },
    [H],
    LenBytes,
    Bytes.
str(str(S)) -->
    [H],
    { H in 0b1010_0000..0b1011_1111,
      L is H /\ 0b0001_1111,
      L in 0..31 },
    msgpack_string_len(S, L).
str(str(S)) -->
    [0xd9,L],
    msgpack_string_len(S, L).
str(str(S)) -->
    [0xda,A,B],
    { L is A<<8 + B },
    msgpack_string_len(S, L).
str(str(S)) -->
    [0xdb,A,B,C,D],
    { L is A<<24 + B<<16 + C<<8 + D },
    msgpack_string_len(S, L).

% Bins i.e. byte arrays
bin(bin(Data)) -->
    [0xc4, Len|Data],
    { length(Data, Len) }.
bin(bin(Data)) -->
    [0xc5, A, B|Data],
    { Len is A<<8 + B,
      length(Data, Len) }.
bin(bin(Data)) -->
    [0xc6, A, B, C, D|Data],
    { Len is A<<24 + B<<16 + C<<8 + D,
      length(Data, Len) }.

% Arrays

% Array helper predicates

array_header(L, 0xdc) :- L < 1<<16.
array_header(L, 0xdd) :- L < 1<<32.

array_pad_bytes([B], [0, B]).
array_pad_bytes([A, B], [A, B]).
array_pad_bytes([A,B,C], [0,A,B,C]).
array_pad_bytes([A,B,C,D], [A,B,C,D]).

array(list(List)) -->
    { is_list(List), length(List, Len), Len < 15,
      !,
      H is 0b10010000 + Len },
    [H], msgpack_list_len(List, Len).
array(list(List)) -->
    { is_list(List), length(List, Len), Len < 1<<32,
      !,
      array_header(Len, H),
      int_bytes(Len, LenBytes_),
      array_pad_bytes(LenBytes_, LenBytes) },
    [H],
    LenBytes,
    msgpack_list_len(List, Len).
array(list(List)) -->
    [H],
    { H in 0b1001_0000..0b1001_1111,
      L is H /\ 0b0000_1111,
      L in 0..15 },
    msgpack_list_len(List, L).
array(list(List)) -->
    [0xdc,A,B],
    { Len is A <<8 + B },
    msgpack_list_len(List, Len).
array(list(List)) -->
    [0xdd,A,B,C,D],
    { Len is A <<24 + B<<16 + C<<8 + D },
    msgpack_list_len(List, Len).

msgpack_list_len([], 0, L, L) :- !.
msgpack_list_len([A|As], N) -->
    msgpack(A), { Nn is N - 1 }, msgpack_list_len(As, Nn).

% Maps
% Need to use pairs insead of dicts, because dicts only support atom
% or integer keys

% map helper predicates
msgpack_dict_len([], 0, L, L) :- !.
msgpack_dict_len([K-V|KVs], N) -->
    msgpack(K),
    msgpack(V),
    { Nn is N - 1 },
    msgpack_dict_len(KVs, Nn).

dict_header(L, 0xde) :- L < 1<<16.
dict_header(L, 0xdf) :- L < 1<<32.

map(dict(D)) -->
    { is_list(D), length(D, L), L < 15, !,
      H is 0b10000000 + L },
      [H],
      msgpack_dict_len(D, L).
map(dict(D)) -->
    { is_list(D), length(D, Len), Len < 1<<32, !,
      dict_header(Len, H),
      int_bytes(Len, LenBytes_),
      array_pad_bytes(LenBytes_, LenBytes) },
      [H],
      LenBytes,
      msgpack_dict_len(D, Len).
map(dict(D)) -->
    [H],
    { H in 0b10000000..0b10001111,
      L is H /\ 0b0000_1111 },
      msgpack_dict_len(D, L).
map(dict(D)) -->
    [0xde, A, B],
    { Len is A<<8 + B },
    msgpack_dict_len(D, Len).
map(dict(D)) -->
    [0xdf, A, B, C, D],
    { Len is A<<24 + B<<16 + C<<8 + D },
    msgpack_dict_len(D, Len).

% Extension types

ext(ext(Type, [Data])) -->
    [0xd4, Type, Data],
    { Type in 0..0x7f }.
ext(ext(Type, [A,B])) -->
    [0xd5, Type, A, B],
    { Type in 0..0x7f }.
ext(ext(Type, [A,B,C,D])) -->
    [0xd6, Type, A, B, C, D],
    { Type in 0..0x7f }.
ext(ext(Type, Data)) -->
    [0xd7, Type|Data],
    { Type in 0..0x7f },
    { length(Data, 8) }.
ext(ext(Type, Data)) -->
    [0xd8, Type|Data],
    { Type in 0..0x7f },
    { length(Data, 16) }.
ext(ext(Type, Data)) -->
    [0xc7, Len, Type|Data],
    { Type in 0..0x7f },
    { Len in 0..255,
      length(Data, Len) }.
ext(ext(Type, Data)) -->
    [0xc8, A, B, Type|Data],
    { Type in 0..0x7f },
    { Len #< 1<<17,
      [A,B] ins 0..255,
      Len #= A<<8 + B,
      length(Data, Len) }.
ext(ext(Type, Data)) -->
    [0xc9, A, B, C, D, Type|Data],
    { Type in 0..0x7f },
    { Len #< 1<<33,
      [A,B,C,D] ins 0..255,
      Len #= A<<24 + B<<16 + C<<8 + D,
      length(Data, Len) }.

% Timestamp extensions
% timestamp32 stores number of seconds since 1970-01-01 00:00:00 UTC as uint32
timestamp(dt(Dt)) -->
    { ground(Dt) },
    [0xd6, 0xff, A, B, C, D],
    { date_time_stamp(Dt, Tss),
      Ts is truncate(Tss),
      A is (Ts /\ 0xff00_0000) >> 24,
      B is (Ts /\ 0x00ff_0000) >> 16,
      C is (Ts /\ 0x0000_ff00) >> 8,
      D is (Ts /\ 0x0000_00ff) >> 0 }.
timestamp(dt(T)) -->
    [0xd6, 0xff, A, B, C, D],
    { Ts is A<<24 + B<<16 + C<<8 + D,
      stamp_date_time(Ts, T, 'UTC') }.
% timestamp 64 stores the number of seconds and nanoseconds that have
% elapsed since epoch; nanosecond in a 30-bit unsigned int and seconds
% in a 34-bit unsigned int
timestamp(dt(T)) -->
    % Can't use clp(fd) here, since we need to use floats...
    [0xd7, 0xff, A, B, C, D, E, F, G, H],
    { Tsn is float(A<<22 + B<<14 + C<<6 + (D /\ 0b1111_1100)>>6),
      Tss is (D /\ 0b011) << 32 + E<<24 + F<<16 + G<<8 + H,
      Tsn < 1e9,
      Ts is Tss + (Tsn / 1e9),
      stamp_date_time(Ts, T, 'UTC') }.
% timestamp 96 stores the number of seconds and nanoseconds since
% epoch; nanoseconds in a 32-bit unsigned int and seconds in a 64-bit
% signed int
timestamp(dt(T)) -->
    [0xc7, 12, 0xff, Na, Nb, Nc, Nd, Sa, Sb, Sc, Sd, Se, Sf, Sg, Sh],
    { Tn is float(Na<<24 + Nb<<16 + Nc<<8 + Nd),
      Ts_ is Sa<<56 + Sb<<48 + Sc<<40 +Sd<<32 + Se<<24 + Sf<<16 + Sg<<8 + Sh,
      unsigned64_signed64(Ts_, Ts),
      Tn < 1e9,
      Time is Ts + (Tn / 1e9),
      stamp_date_time(Time, T, 'UTC') }.

% helper predicates
int_bytes(I, B) :- int_bytes(I, [], B).
int_bytes(0, R, R).
int_bytes(I, Bs, R) :-
    Bl is I /\ 0xff,
    In is I >> 8,
    int_bytes(In, [Bl|Bs], R).

unsigned16_signed16(Un, Si) :-
    integer(Un),
    Un >= 0x8000,
    Inv is 0xffff - Un,
    Si is -Inv - 1.
unsigned16_signed16(Un, Si) :-
    integer(Si),
    Si < 0,
    Inv is -Si - 1,
    Un is 0xffff - Inv.
unsigned16_signed16(Un, Un).

unsigned32_signed32(Un, Si) :-
    integer(Un),
    Un >= 0x8000_0000,
    Inv is 0xffff_ffff - Un,
    Si is -Inv - 1.
unsigned32_signed32(Un, Si) :-
    integer(Si),
    Si < 0,
    Inv is -Si - 1,
    Un is 0xffff_ffff - Inv.
unsigned32_signed32(Un, Un).

unsigned64_signed64(Un, Si) :-
    integer(Un),
    Un >= 0x8000_0000_0000_0000,
    Inv is 0xffff_ffff_ffff_ffff - Un,
    Si is -Inv - 1.
unsigned64_signed64(Un, Si) :-
    integer(Si),
    Si < 0,
    Inv is -Si - 1,
    Un is 0xffff_ffff_ffff_ffff - Inv.
unsigned64_signed64(Un, Un).

% :- use_module(library(plunit)).
% ?- load_test_files([]), run_tests.
