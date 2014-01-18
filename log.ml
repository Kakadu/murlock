open Printf

let filename = ref "log"
;;
let ch = ref(open_out !filename)
;; 
let init str =
  filename:=str;
  close_out !ch;
  ch := open_out !filename
;;
let put str = fprintf !ch "%s" str;;

let flush = flush !ch;;

let close = close_out !ch;; 
