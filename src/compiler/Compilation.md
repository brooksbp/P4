# Compilation

## Header Type Decls

This header:

```
header_type h1 {
    fields {
        f1 : 12;
        f2 : 1;
        f3 : 22;
        f4 : 5;
    }
}
```

has this layout:

```
             "f1"   "f2"         "f3"           "f4"
        /-----------\|/-----------------------\/---\
        |           |||                       ||   |
        0          11||                       ||   |
bit                 12|                       ||   |
                     13                      34|   |
                                              35  39

        11111111 00001111 11111111 11111111 11111111

byte[]     0        1        2        3         4
```

We need to support operations on header fields such as:

* get, set
* compare
* add, sub

And, there are no constraints on max number of bits per field.

So, we'll represent a field as a bit vector.  The reference implementation is
"Extended-Precision Arithmetic" from C Interfaces and Implementations.  These
bit vectors will support any operations we'll need.

```
f1 value = 111111110000 = 4080

represented as a polynomial of an 8-bit base:

          00001111 * (1<<8)^1  +  11110000 * (1<<8)^0
                15 * 256       +       240 * 1
                              4080
```

Therefore, "f1" will have the following bit vector layout:

```
          xxxx1111 11110000
byte[]        1        0
```

The translation from the header layout to the bit vector layout is
conceptually straightforward: take the header bits, shift them right until
they're byte-aligned, then reverse the bytes.

Each field of a header instance will also have a corresponding bit vector.

Future Optimizations:

* Fields less than or equal to 64 bits can be translated to and from a C/machine
native type.  E.g. "f4" can be represented as a `uint8_t` which will be more
efficient in terms of space and time.
