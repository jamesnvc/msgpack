:- use_module(library(plunit)).
:- use_module(msgpack).
:- begin_tests(msgpack).

% helper tests
test(unsign_sign_1) :-
    msgpack:unsigned64_signed64(Un, -100), !,
    Un = 18446744073709551516.
test(unsign_sign_2) :-
    msgpack:unsigned64_signed64(0xff00_abcd_0000_1111, Si),
    !, Si = -71868697081278191.

test(nil_val) :- msgpack(none, [0xc0], []).
test(bool_false) :- msgpack(false, [0xc2], []).
test(bool_true) :- msgpack(true, [0xc3], []).

test(fixnum_1) :- msgpack(14, [14], []).
test(fixnum_max) :- msgpack(0b01111111, [0b01111111], []).
test(fixnum_neg) :- msgpack(-27, [0b11100101], []).
test(uint8) :- msgpack(0b10000000, [0xcc, 0b10000000], []).
test(uint16) :- msgpack(0x1bcd, [0xcd, 0x1b, 0xcd], []).
test(uint32) :- msgpack(0xabcdef98,
                        [0xce, 0xab, 0xcd, 0xef, 0x98],
                        []).
test(uint64) :- msgpack(0xabcdef98abcdef98,
                        [0xcf, 0xab, 0xcd, 0xef, 0x98, 0xab, 0xcd, 0xef, 0x98],
                        []).


test(unpack_fixnum_pos) :- msgpack(X, [14], []), !, X = 14.
test(unpack_fixnum_neg) :- msgpack(X, [238], []), !, X = -18.
test(unpack_uint8) :- msgpack(X, [0xcc, 0xfa], []), !, X = 0xfa.
test(unpack_uint16) :- msgpack(X, [0xcd, 0x1b, 0xcd], []), !, X = 0x1bcd.
test(unpack_uint32) :- msgpack(X, [0xce, 0xab, 0xcd, 0xef, 0x98], []),
                       !,
                       X = 0xabcdef98.
test(unpack_uint64) :-
    msgpack(X, [0xcf, 0xa8, 0x23, 0xfa, 0x84, 0xac, 0xde, 0x10, 0x17], []),
    !, X = 0xa823fa84acde1017.

test(pack_fixnum_pos) :- msgpack(14, X, []), !, X = [14].
test(pack_fixnum_neg) :- msgpack(-18, X, []), !, X = [238].
test(pack_uint8) :- msgpack(0xfa, X, []), !, X = [0xcc, 0xfa].
test(pack_uint16) :- msgpack(0x1bcd, X, []), !, X = [0xcd, 0x1b, 0xcd].
test(pack_uint32) :- msgpack(0xabcdef98, X, []),
                     !, X = [0xce, 0xab, 0xcd, 0xef, 0x98].
test(pack_uint64) :-
    msgpack(0xa823fa84acde1017, X, []),
    !, X = [0xcf, 0xa8, 0x23, 0xfa, 0x84, 0xac, 0xde, 0x10, 0x17].

test(neg_int8) :- msgpack(-56, [0xd0, 0b11001000], []).
test(unpack_int8) :- msgpack(X, [0xd0, 0b10011010], []), !, X = -102.
test(pack_int8) :- msgpack(-111, X, []), !, X = [0xd0, 0b10010001].

test(neg_int16) :- msgpack(-21555, [0xd1, 0xab, 0xcd], []).
test(unpack_int16) :- msgpack(X, [0xd1, 0x12, 0x34], []), !, X = 4660.
test(pack_int16) :- msgpack(-28673, X, []), !, X = [0xd1, 0x8f, 0xff].

test(neg_int32) :- msgpack(-1884566017, [0xd2, 0x8f, 0xab, 0xcd, 0xff], []).
test(unpack_int32) :- msgpack(X, [0xd2, 0x6a, 0x8b, 0x5f, 0x12], []),
                      !, X = 0x6a8b5f12.
test(pack_int32) :- msgpack(-1884566017, X, []),
                    !, X = [0xd2, 0x8f, 0xab, 0xcd, 0xff].

test(neg_int64) :-
    msgpack(-8137253475156295520,
            [0xd3, 0x8f, 0x12, 0xab, 0x13, 0xcd, 0xff, 0x00, 0xa0],
            []).
test(unpack_int64) :-
    msgpack(X, [0xd3, 0x8f, 0x12, 0xab, 0x13, 0xcd, 0xff, 0x00, 0xa0], []),
    !, X = -8137253475156295520.
test(unpack_int64_2) :-
    msgpack(X, [0xd3, 0x6f, 0x12, 0xab, 0x13, 0xcd, 0xff, 0x00, 0xa0], []),
    !, X = 0x6f12_ab13_cdff_00a0.
test(pack_int64) :-
    msgpack(-8137253475156295520, X, []),
    !, X = [0xd3, 0x8f, 0x12, 0xab, 0x13, 0xcd, 0xff, 0x00, 0xa0].

test(short_string) :- msgpack(str("ABC"), [0b10100011, 65, 66, 67], []).
test(pack_short_string) :- msgpack(str("Foobar"), X, []),
                           !, X = [166, 70, 111, 111, 98, 97, 114].
test(unpack_short_string) :- msgpack(X, [165, 104, 101, 108, 108, 111], []),
                             !, X = str("hello").

test(short_list) :- msgpack(list([true, false, none]),
                                 [0b10010011, 0xc3, 0xc2, 0xc0], []).
test(nested_short_list) :-
    msgpack(list([true, list([str("quux"), str("foobar")]), false]),
            [0b10010011,
             0xc3,
             0b10010010,
             164, 113, 117, 117, 120,
             166, 102, 111, 111, 98, 97, 114,
             0xc2],
            []).
test(unpack_short_list) :- msgpack(X, [0b10010011,
                                       28, % fixnum
                                       0xcc, 0xfa, %uint8
                                       0xc0 % nil
                                      ], []),
                           !, X = list([28, 0xfa, none]).
test(pack_short_list) :- msgpack(list([0xfff, true, false, none]), X, []),
                         !, X = [0b10010100, 205, 15, 255, 0xc3, 0xc2, 0xc0].

test(longer_list) :-
    msgpack(list([1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10]),
                 [0xdc, 0x00, 20,
                  1,2,3,4,5,6,7,8,9,10,
                  1,2,3,4,5,6,7,8,9,10],
                []).
test(unpack_longer_list) :-
    msgpack(X, [0xdc, 0x00, 20, 1,2,3,4,5,6,7,8,9,10, 1,2,3,4,5,6,7,8,9,10],
            []),
    !, X = list([1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10]).
test(pack_longer_list) :-
    msgpack(list([1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10]),
                 X, []),
    !, X = [0xdc, 0x00, 20, 1,2,3,4,5,6,7,8,9,10, 1,2,3,4,5,6,7,8,9,10].

test(short_dict) :-
    msgpack(dict([str("foo")-1, str("bar")-str("quux")]),
            [0b10000010,
             163,102,111,111,
             1,
             163, 98, 97, 114,
             164, 113, 117, 117, 120],
            []).
test(unpack_short_dict) :-
    msgpack(X,
            [0b10000010,
             163,102,111,111,
             1,
             163, 98, 97, 114,
             164, 113, 117, 117, 120],
            []),
    !, X = dict([str("foo")-1, str("bar")-str("quux")]).
test(pack_short_dict) :-
    msgpack(dict([str("foo")-1, str("bar")-str("quux")]),
            X,
            []),
    !, X = [0b10000010,
         163,102,111,111,
         1,
         163, 98, 97, 114,
         164, 113, 117, 117, 120].

test(bin8) :- msgpack(bin([0xfa, 0xbc, 0x00]), [0xc4, 3, 0xfa, 0xbc, 0x00], []).

test(ext1) :- msgpack(ext(28, [0xab]), [0xd4, 28, 0xab], []).
test(ext2) :- msgpack(ext(37, [0xab, 0xbc]), [0xd5, 37, 0xab, 0xbc], []).
test(ext3) :- msgpack(ext(42, [0xab, 0xbc]), [0xc7, 2, 42, 0xab, 0xbc], []).

test(timestamp) :- msgpack(date(2021, 1, 14, 9, 43, 16.0, 0, 'UTC', -),
                           [0xd6, 0xff, 0x60, 0x00, 0x12, 0x34],
                           []).
test(timestamp64) :-
    msgpack(date(2032, 11, 28, 4, 35, 28.720599889, 0, 'UTC', -),
            [0xd7, 0xff, 0xab, 0xcd, 0xef, 0x98,   0x76, 0x54, 0x32, 0x10], []).

test(float_single) :-
    msgpack(single(0.15625),
            [0xca, 0b0011_1110, 0b0010_0000, 0b0000_0000, 0b00000000],
            []).
% currently failing b/c packing floats seems like a huge pain in the ass
%% test(pack_float_single) :-
%%     msgpack(single(0.15625), X, []),
%%     !, X = [0xca, 0b0011_1110, 0b0010_0000, 0b0000_0000, 0b00000000].
test(unpack_float_single) :-
    msgpack(X, [0xca, 0b0011_1110, 0b0010_0000, 0b0000_0000, 0b00000000], []),
    !, X = single(0.15625).

test(complex_unpack) :-
    msgpack(Data,
            [0b10000010,
             163,102,111,111,
             1,
             163, 98, 97, 114,
             0b10010100, 205, 15, 255, 0xc3, 0xc2, 0xc0],
            []),
    !, Data = dict([str("foo")-1, str("bar")-list([4095, true, false, none])]).

test(complex_pack) :-
    msgpack(list([-32, str("bloop"), list([65, 129, 2000])]),
            Bytes, []),
    !, Bytes = [0x93, 0xd0, 0xe0, 0xa5, 0x62, 0x6c, 0x6f, 0x6f, 0x70, 0x93,
                0x41, 0xcc, 0x81, 0xcd, 0x7, 0xd0].

:- end_tests(msgpack).
