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

test(str8_upto_255) :-
   findall( C, ( between(1, 255, _), C is random(94) ), Codes),
   string_codes(S,Codes),
   phrase(msgpack(str(S)), Bs),
   assertion(Bs = [ 0xd9, 255 | Codes ]).

test(str16_upto_65535) :-
   findall( C, ( between(1, 65535, _), C is random(94) ), Codes),
   string_codes(S,Codes),
   phrase(msgpack(str(S)), Bs),
   assertion(Bs = [ 0xda, 255, 255 | Codes ]).

test(str32_upto_4294967295) :-
   findall( C, ( between(1, 100_000, _), C is random(94) ), Codes),
   string_codes(S,Codes),
   phrase(msgpack(str(S)), Bs),
   assertion(Bs = [ 0xdb, 0, 1, 134, 160 | Codes ]).

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

test(pack_array_byte_range) :-
   findall( C,  between(0, 127, C), Ls),
   phrase(msgpack(list(Ls)), Bs),
   assertion(Bs = [ 0xdc, 0x0, 0x80 | Ls ]),
   findall( C,  between(128, 255, C), Ls1),
   findall( [204,C],  between(128, 255, C), LsExpected0),
   flatten(LsExpected0, LsExpected),
   phrase(msgpack(list(Ls1)), Bs1),
   assertion(Bs1 = [ 0xdc, 0x0, 0x80 | LsExpected ]).

test(pack_array32_upto_4294967295) :-
   findall( C, ( between(1, 111_333, _), C is random(128) ), Ls),
   phrase(msgpack(list(Ls)), Bs),
   assertion(Bs = [ 0xdd, 0, 0x1, 0xb2, 0xe5 | Ls ]).


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

test(map16_more_than_15_pairs_dict) :-
   findall(str(K)-V, 
           ( between(1,16,V), 
             format(string(K), '~w',[V]) ),
           KVs),
   msgpack(dict(KVs), Bs, []),
   msgpack(KVs1, Bs, []),
   assertion(KVs1 == dict(KVs)).

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

test(big_test) :-
    msgpack(Ms,
            [136,164,126,58,105,100,146,163,126,35,117,146,207,90,52,146,183,254,
             149,75,120,211,181,115,43,192,200,203,18,232,169,126,58,99,111,110,116,
             101,110,116,165,104,101,108,108,111,171,126,58,116,104,114,101,97,100,
             45,105,100,146,163,126,35,117,146,207,90,52,127,237,227,10,78,12,211,
             158,88,156,157,65,116,128,173,170,126,58,103,114,111,117,112,45,105,
             100,146,163,126,35,117,146,207,90,52,127,181,161,118,68,147,211,178,
             241,12,63,156,43,65,103,169,126,58,117,115,101,114,45,105,100,146,163,
             126,35,117,146,207,90,52,127,181,161,144,68,155,211,178,248,74,147,2,78,
             25,217,172,126,58,99,114,101,97,116,101,100,45,97,116,146,163,126,35,
             109,207,0,0,1,96,93,93,30,44,180,126,58,109,101,110,116,105,111,110,101,
             100,45,117,115,101,114,45,105,100,115,146,166,126,35,108,105,115,116,
             144,179,126,58,109,101,110,116,105,111,110,101,100,45,116,97,103,45,105,
             100,115,146,166,126,35,108,105,115,116,144],
            []),!,
    Ms = dict([str("~:id")-list([str("~#u"), list([6499981481150335864, -5371901823520402712])]),
               str("~:content")-str("hello"),
               str("~:thread-id")-list([str("~#u"), list([6499960821895548428,
                                                          -7036702218544447315])]),
               str("~:group-id")-list([str("~#u"), list([6499960580277159059,
                                                         -5552643398228557465])]),
               str("~:user-id")-list([str("~#u"), list([6499960580278863003,
                                                        -5550604545474815527])]),
               str("~:created-at")-list([str("~#m"), 1513394871852]),
               str("~:mentioned-user-ids")-list([str("~#list"), list([])]),
               str("~:mentioned-tag-ids")-list([str("~#list"), list([])])]).

test(speed_test) :-
   Bs=[148, 1, 14, 192, 220, 0, 17, 131, 162, 105, 100, 2, 166, 115, 116, 114,
       101, 97, 109, 166, 115, 116, 100, 101, 114, 114, 164, 109, 111, 100 , 101, 165,
       98, 121, 116, 101, 115, 132, 162, 105, 100, 3, 166, 115, 116, 114, 101, 97,
       109, 163, 106, 111, 98, 166, 99, 108, 105, 101, 110, 116, 133, 164, 110, 97,
       109, 101, 179, 112, 121, 116, 104, 111, 110, 51, 45, 115, 99, 114, 105, 112,
       116, 45, 104, 111, 115, 116, 167, 118, 101, 114, 115, 105, 111, 110, 132, 165,
       109, 97, 106, 111, 114, 0, 165, 109, 105, 110, 111, 114, 3, 165, 112, 97, 116,
       99, 104, 1, 170, 112, 114, 101, 114, 101, 108, 101, 97, 115, 101, 160, 164,
       116, 121, 112, 101, 164, 104, 111, 115, 116, 167, 109, 101, 116, 104, 111, 100,
       115, 131, 164, 112, 111, 108, 108, 128, 165, 115, 112, 101, 99, 115, 129, 165,
       110, 97, 114, 103, 115, 1, 168, 115, 104, 117, 116, 100, 111, 119, 110, 128,
       170, 97, 116, 116, 114, 105, 98, 117, 116, 101, 115, 130, 167, 108, 105, 99,
       101, 110, 115, 101 , 169, 65, 112, 97, 99, 104, 101, 32, 118, 50, 167, 119,
       101, 98, 115, 105, 116, 101, 191, 103, 105, 116, 104, 117, 98, 46, 99, 111,
       109, 47, 110, 101, 111, 118, 105, 109, 47, 112, 121, 116, 104, 111, 110, 45,
       99, 108, 105, 101, 110, 116, 164, 109, 111, 100, 101, 163, 114, 112, 99, 131,
       162, 105, 100, 4, 166, 115, 116, 114 , 101, 97, 109, 163, 106, 111, 98, 164,
       109, 111, 100, 101, 165, 98, 121, 116, 101, 115, 131, 162, 105, 100, 5, 166,
       115, 116, 114, 101, 97, 109, 163, 106, 111, 98, 164, 109, 111, 100, 101, 165,
       98, 121, 116, 101, 115, 132, 162, 105, 100, 6, 166, 115, 116, 114, 101, 97,
       109, 166, 115, 111, 99, 107, 101, 116, 166, 99, 108, 105, 101, 110, 116, 128,
       164, 109, 111, 100, 101, 163, 114, 112, 99, 131, 162, 105, 100, 7, 166, 115,
       116, 114, 101, 97, 109, 163, 106, 111, 98, 164, 109, 111, 100, 101, 165, 98,
       121, 116, 101, 115, 131, 162, 105, 100, 8, 166, 115, 116, 114, 101, 97, 109,
       163, 106, 111, 98, 164, 109, 111, 100, 101, 165, 98, 121, 116, 101 , 115, 132,
       162, 105, 100, 9, 166, 115, 116, 114, 101, 97, 109, 166, 115, 111, 99, 107,
       101, 116, 166, 99, 108, 105, 101, 110, 116, 128, 164, 109, 111, 100, 101, 163,
       114, 112, 99, 132, 162, 105, 100, 10, 166, 115, 116, 114, 101, 97, 109, 166,
       115, 111, 99, 107, 101, 116, 166, 99, 108, 105, 101, 110, 116, 128, 164, 109,
       111, 100, 101, 163, 114, 112, 99, 132, 162, 105, 100, 204, 192, 166, 115, 116,
       114, 101, 97, 109, 166, 115, 111, 99, 107, 101, 116, 166, 99, 108, 105, 101,
       110, 116, 128, 164, 109, 111, 100, 101, 163, 114, 112, 99, 132, 162, 105, 100,
       204, 193, 166, 115, 116, 114, 101, 97, 109, 166, 115, 111, 99, 107, 101, 116,
       166, 99, 108, 105, 101, 110, 116, 128, 164, 109, 111, 100, 101, 163, 114, 112,
       99, 132, 162, 105, 100, 204, 194, 166, 115, 116, 114, 101, 97, 109, 166, 115,
       111, 99, 107, 101, 116, 166, 99, 108, 105, 101, 110, 116, 128, 164, 109, 111,
       100, 101, 163, 114, 112, 99, 132, 162, 105, 100, 204, 195, 166, 115, 116, 114,
       101, 97, 109, 166 , 115, 111, 99, 107, 101, 116, 166, 99, 108, 105, 101, 110,
       116, 128, 164, 109, 111, 100, 101, 163, 114, 112, 99, 132, 162, 105, 100, 204,
       196, 166, 115, 116, 114, 101, 97, 109, 166, 115, 111, 99, 107, 101, 116, 166,
       99, 108, 105, 101, 110, 116, 128, 164, 109, 111, 100, 101, 163, 114, 112, 99,
       132, 162, 105, 100, 204, 197, 166, 115, 116, 114, 101, 97, 109, 166, 115, 111,
       99, 107, 101, 116, 166, 99, 108, 105, 101, 110, 116, 128, 164, 109, 111, 100,
       101, 163, 114, 112, 99, 132, 162, 105, 100, 204, 205, 166, 115, 116, 114, 101,
       97, 109, 166, 115, 111, 99, 107, 101, 116, 166, 99, 108, 105, 101, 110, 116,
       128, 164, 109, 111, 100, 101, 163, 114, 112, 99, 132, 162, 105, 100, 204, 206,
       166, 115, 116, 114, 101, 97, 109, 166, 115, 111, 99, 107, 101, 116, 166, 99,
       108, 105, 101, 110, 116, 128, 164, 109, 111, 100, 101, 163, 114, 112, 99],
   get_time(T0),
   phrase(msgpack(MPRes), Bs ),
   get_time(T1),
   MSec is (T1-T0)*1000,
   print_message(informational, format("msgpack: Unpack speed test finished in ~3f ms.", [MSec])),
   % Make sure unpacking this test takes less than 50ms, this may need to be
   % changed if we are on a very slow cpu
   assertion(MSec < 50),
   MPExpected = list([1, 14, none, list([dict([str("id")-2,
          str("stream")-str("stderr"), str("mode")-str("bytes")]),
          dict([str("id")-3, str("stream")-str("job"),
          str("client")-dict([str("name")-str("python3-script-host"),
          str("version")-dict([str("major")-0, str("minor")-3, str("patch")-1,
          str("prerelease")-str("")]), str("type")-str("host"),
          str("methods")-dict([str("poll")-dict([]), str("specs")-dict([str("nargs")-1]),
          str("shutdown")-dict([])]), str("attributes")-dict([str("license")-str("Apache v2"), 
          str("website")-str("github.com/neovim/python-client")])]),
          str("mode")-str("rpc")]), dict([str("id")-4, str("stream")-str("job"),
          str("mode")-str("bytes")]), dict([str("id")-5, str("stream")-str("job"),
          str("mode")-str("bytes")]), dict([str("id")-6, str("stream")-str("socket"),
          str("client")-dict([]), str("mode")-str("rpc")]), dict([str("id")-7,
          str("stream")-str("job"), str("mode")-str("bytes")]), dict([str("id")-8,
          str("stream")-str("job"), str("mode")-str("bytes")]),
          dict([str("id")-9, str("stream")-str("socket"), str("client")-dict([]),
          str("mode")-str("rpc")]), dict([str("id")-10, str("stream")-str("socket"),
          str("client")-dict([]), str("mode")-str("rpc")]), dict([str("id")-192,
          str("stream")-str("socket"), str("client")-dict([]), str("mode")-str("rpc")]),
          dict([str("id")-193, str("stream")-str("socket"), str("client")-dict([]),
          str("mode")-str("rpc")]), dict([str("id")-194, str("stream")-str("socket"),
          str("client")-dict([]), str("mode")-str("rpc")]), dict([str("id")-195,
          str("stream")-str("socket"), str("client")-dict([]), str("mode")-str("rpc")]),
          dict([str("id")-196, str("stream")-str("socket"), str("client")-dict([]),
          str("mode")-str("rpc")]), dict([str("id")-197, str("stream")-str("socket"),
          str("client")-dict([]), str("mode")-str("rpc")]), dict([str("id")-205,
          str("stream")-str("socket"), str("client")-dict([]), str("mode")-str("rpc")]),
          dict([str("id")-206, str("stream")-str("socket"), str("client")-dict([]),
          str("mode")-str("rpc")])])]),
   assertion(MPRes == MPExpected).
:- end_tests(msgpack).
