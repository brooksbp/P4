#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

struct h1_instance {
  uint8_t *bytes;  // Pointer to an h1 instance data; assigned by parser.
  uint8_t f1[2];
  uint8_t f2[1];
  uint8_t f3[3];
  uint8_t f4[1];
};
const int h1_instance_len = 5;
const int f1_len = 2;
const int f2_len = 1;
const int f3_len = 3;
const int f4_len = 1;

// copy bits from header layout to xp layout
void to_xp(uint8_t *bytes, uint8_t *xp, int offset, int nbits) {
  int bit;
  for (bit = 0; bit < nbits; bit++) {
    int header_bit = offset + nbits - 1 - bit;
    int header_bit_value = (bytes[header_bit / 8] >> (7 - (header_bit % 8))) & 0x01;
    xp[bit / 8] |= header_bit_value << (bit % 8);
  }
}

// copy bits from xp layout to header layout
void from_xp(uint8_t *bytes, uint8_t *xp, int offset, int nbits) {
  int bit;
  for (bit = 0; bit < nbits; bit++) {
    int xp_bit_value = (xp[bit / 8] >> (bit % 8)) & 0x01;
    int header_bit = offset + nbits - 1 - bit;
    bytes[header_bit / 8] &= ~(1 << (7 - (header_bit % 8)));
    bytes[header_bit / 8] |= xp_bit_value << (7 - (header_bit % 8));
  }
}

void h1_to_xp(struct h1_instance *h1) {
  to_xp(h1->bytes, h1->f1, 0, 12);
  to_xp(h1->bytes, h1->f2, 12, 1);
  to_xp(h1->bytes, h1->f3, 13, 22);
  to_xp(h1->bytes, h1->f4, 35, 5);
}

void h1_from_xp(struct h1_instance *h1) {
  from_xp(h1->bytes, h1->f1, 0, 12);
  from_xp(h1->bytes, h1->f2, 12, 1);
  from_xp(h1->bytes, h1->f3, 13, 22);
  from_xp(h1->bytes, h1->f4, 35, 5);
}

static uint8_t pkt[] = { 0xFF, 0x0F, 0xFF, 0xFF, 0xFF };
static uint8_t xxx[] = { 0xFF, 0xFF, 0xFF, 0xFF, 0xFF };

static struct h1_instance h1 = {
  .bytes = pkt,
};

int main() {

  int i;

  for (i = f1_len - 1; i >= 0; i--) printf("%02x ", h1.f1[i]); printf("\n");
  for (i = f2_len - 1; i >= 0; i--) printf("%02x ", h1.f2[i]); printf("\n");
  for (i = f3_len - 1; i >= 0; i--) printf("%02x ", h1.f3[i]); printf("\n");
  for (i = f4_len - 1; i >= 0; i--) printf("%02x ", h1.f4[i]); printf("\n");
  printf("\n");

  h1_to_xp(&h1);

  for (i = f1_len - 1; i >= 0; i--) printf("%02x ", h1.f1[i]); printf("\n");
  for (i = f2_len - 1; i >= 0; i--) printf("%02x ", h1.f2[i]); printf("\n");
  for (i = f3_len - 1; i >= 0; i--) printf("%02x ", h1.f3[i]); printf("\n");
  for (i = f4_len - 1; i >= 0; i--) printf("%02x ", h1.f4[i]); printf("\n");
  printf("\n");

  h1.bytes = xxx;

  h1_from_xp(&h1);

  for (i = 0; i < 5; i++) printf("%02x ", h1.bytes[i]); printf("\n");
  printf("\n");

  return 0;
}
