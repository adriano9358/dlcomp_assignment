#include <stdio.h>
#include "mem_runtime.h"

int main() {
    ref r = new_ref_int(42);
    ref x = new_ref_ref(r);
    int v = deref_int(deref_ref(x));
    free_ref(deref_ref(x));
    free_ref(x);
    printf("%d\n", v);
    // printf("%d\n", deref_int(deref_ref(r2)));
    // assign_int(x, v + 1);
    // ref r2 = new_ref_ref(x);
    return 0;
}