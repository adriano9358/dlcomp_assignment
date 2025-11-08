#include <stdbool.h>

typedef void* ref;

ref new_ref_int(int v);
ref new_ref_bool(bool b);
ref new_ref_ref(ref r);

int deref_int(ref r);
bool deref_bool(ref r);
ref deref_ref(ref r);

void assign_int(ref r, int v);
void assign_bool(ref r, bool v);
void assign_ref(ref r, ref v);

void free_ref(ref r);