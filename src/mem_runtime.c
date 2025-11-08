#include <stdlib.h>
#include "mem_runtime.h"

typedef void* ref;

ref new_ref_int(int v) {
    int* p = (int*)malloc(sizeof(int));
    *p = v;
    return (ref)p;
}
ref new_ref_bool(bool b) {
    bool* p = (bool*)malloc(sizeof(bool));
    *p = b;
    return (ref)p;
}
ref new_ref_ref(ref r) {
    ref* p = (ref*)malloc(sizeof(ref));
    *p = r;
    return (ref)p;
}

int deref_int(ref r) {
    return *(int*)r;
}
bool deref_bool(ref r) {
    return *(bool*)r;
}
ref deref_ref(ref r) {
    return *(ref*)r;
}

void assign_int(ref r, int v) {
    *(int*)r = v;
}
void assign_bool(ref r, bool v) {
    *(bool*)r = v;
}
void assign_ref(ref r, ref v) {
    *(ref*)r = v;
}

void free_ref(ref r) {
    free(r);
}