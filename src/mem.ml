
let new_ref v1 = ref v1

let deref r = !r

let assign r v = r := v

let free _ = ()