open Color

let lastflag = ref 0

let timelimit = 60.0 

let remaining_time = ref 0.0

let start_time = ref 0.0

let my_time = ref 0 

let rotate = ref 0

let kill_flag = ref 0

let waste_time = ref 0.

let middle = 44

let last_max = 22

let last_rest = ref 0.

let last = ref last_max

let turncolor = ref black

let depth_middle = 12

let depth_m_default = 14

let depth_middle_max = 14

let middle_l_default = 1.5

let middle_limit = ref 1.5

let middle_div = ref 0.

let change_depth = 0.5

let fastfirst = -1

let alpha = -100000
let beta  = 100000

let bplist = [| 45;-11 ;  4; -1; -1;  4;-11; 45;
              -11;-16; -1; -3; -3; -1;-16;-11;
                4; -1;  2; -1; -1;  2; -1;  4;
               -1; -3; -1;  0;  0; -1; -3; -1;
               -1; -3; -1;  0;  0; -1; -3; -1;
                4; -1;  2; -1; -1;  2; -1;  4;
              -11;-16; -1; -3; -3; -1;-16;-11;
               45;-11;  4; -1; -1;  4;-11; 45|]

let wbp = 1
let wfs = 75
let wcn = 15