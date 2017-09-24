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

has layout:

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

### Compilation

See `example_header_type_decl.c` for an idea of auto-generated code for
translation between header type layout and bit vector layout.

## Instances, Parsers, Control Functions, Tables

```
header_type ethernet_t {
  fields {
    dstAddr : 48;
    srcAddr : 48;
    etherType : 16;
  }
}

header ethernet_t ethernet;

parser parse_ethernet {
  extract(ethernet);
  return select(latest.etherType) {
    0x8100: parse_vlan;
    default: egress;
  }
}

header_type vlan_t {
  fields {
    pcp        : 3;
    cfi        : 1;
    vid        : 12;
    ethertype  : 16;
  }
}

header vlan_t vlans[2];

parser parse_vlan {
  extract(vlans[next]);
  return select(latest.etherType) {
    0x8100, 0x9100, 0x9200: parse_vlan;
    default: egress;
  }
}

parser start {
  return parse_ethernet;
}

control egress {
  apply(rewrite_mac);
}

/* silly table+action to match on srcAddr and do a rewrite on hit */

table rewrite_mac {
  reads {
    ethernet.srcAddr : exact;
  }
  actions {
    on_miss;
    rewrite_src_dst_mac;
  }
  size: 1024;
}

action on_miss() {
}

action rewrite_src_dst_mac(smac, dmac) {
  modify_field(ethernet.srcAddr, smac);
  modify_field(ethernet.dstAddr, dmac);
}
```

```c
struct ethernet_t {
  uint8_t *bytes;
  uint8_t dstAddr[6];
  uint8_t srcAddr[6];
  uint8_t etherType[2];
}

struct vlan_t {
  uint8_t *bytes;
  uint8_t pcp[1];
  uint8_t cfi[1];
  uint8_t vid[1];
  uint8_t ethertype[2];
}

struct packet_instance_t {
  uint8_t *bytes;
  int len;

  int current_offset;

  struct ethernet_t ethernet;
  int ethernet_valid;

  struct vlan_t vlans[2];
  int vlans_idx;
  int vlans_valid[2];
}

void parser_start(struct packet_instance_t *p) {
  return parser_parse_ethernet(p);
}

void parser_parse_ethernet(struct packet_instance_t *p) {
  // extract(ethernet)
  p->ethernet.bytes = p->bytes[p->current_offset];
  ethernet_t_to_xp(p->ethernet);

  p->ethernet_valid = 1;
  p->current_offset += ethernet_t_len;

  if (0 == xp_cmp(p->ethernet.etherType, 0x8100))
    return parser_parse_vlan(p);
  return control_egress(p);
}

void parser_parse_vlan(struct packet_instance_t *p) {
  // extract(vlans[next])
  int i = p->vlans_idx;
  p->vlans[i].bytes = p->bytes[p->current_offset];
  p->vlans_idx++;
  vlan_t_to_xp(p->vlans[i]);

  p->vlans_valid[i] = 1;
  p->current_offset += vlan_t_len;

  if (0 == xp_cmp(p->vlans[i], 0x8100))
    return parser_parse_vlan(p);
  if (0 == xp_cmp(p->vlans[i], 0x9100))
    return parser_parse_vlan(p);
  if (0 == xp_cmp(p->vlans[i], 0x9200))
    return parser_parse_vlan(p);
  return control_egress(p);
}

void control_egress(struct packet_instance_t *p) {
  
}
```

