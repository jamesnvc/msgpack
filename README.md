# Prolog-MessagePack

This library provides a DCG, `msgpack//1` for the parsing of MessagePack.

It can be used to either pack or unpack MessagePack data structures to/from bytes.

It is [available from the SWI Prolog registry](http://www.swi-prolog.org/pack/list?p=msgpack) and can be installed via `?- pack_install(msgpack).`.

## Examples

Unpacking:

```prolog
?- Bytes = [0x82, 0xa3, 0x66, 0x6f, 0x6f, 0x1, 0xa3, 0x62, 0x61, 0x72, 0x94,
            0xcd, 0xf, 0xff, 0xc3, 0xc2, 0xc0],
   msgpack(Data, Bytes, []).
Data = dict([str("foo")-1, str("bar")-list([4095, true, false, none])]).
```

Packing:
```prolog
?- msgpack(list([-32, str("bloop"), list([65, 129, 2000])]),
           Bytes, []), format_hex_list(Bytes).
Bytes = [147, 208, 224, 165, 98, 108, 111, 111, 112|...].
```
