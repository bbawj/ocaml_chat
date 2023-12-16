open Unix

let orig_term = ref (tcgetattr stdin)

let leave_raw_mode () = tcsetattr stdin TCSAFLUSH !orig_term

let enter_raw_mode () = 
  at_exit leave_raw_mode;

  let term = !orig_term in
  term.c_echo <- false;
  term.c_icanon <- false;
  term.c_brkint <- false;
  term.c_icrnl <- false;
  term.c_inpck <- false;
  term.c_istrip <- false;
  term.c_ixon <- false;
  term.c_vtime <- 0;
  term.c_vmin <- 1;
  tcsetattr stdin TCSAFLUSH term

let clear_line () = output_string Stdlib.stdout "\o033[2K"; flush Stdlib.stdout
let return_line () = output_char Stdlib.stdout '\r'; flush Stdlib.stdout

let input_buf = Buffer.create 1024

let input_add_char c = Buffer.add_char input_buf c
let input_remove_char () = 
  let len = Buffer.length input_buf in
  if len > 0 then Buffer.truncate input_buf (len - 1)
let input_to_oc oc = Buffer.output_buffer oc input_buf; flush oc
let input_hide () = clear_line (); return_line ()
let input_show () = input_to_oc Stdlib.stdout

let input_latest () = if Buffer.length input_buf > 0 then begin
  let latest = Buffer.nth input_buf (Buffer.length input_buf - 1) in
  print_char latest; flush Stdlib.stdout;
end
let input_clear () = Buffer.reset input_buf; input_hide ()

type input_response = 
  | Ok
  | Gotline

let input_feed_byte (b: int) : input_response = match b with 
  (* ignore \n and handle \r instead *)
  | 10 -> Ok
  (* \r *)
  | 13 -> input_add_char '\n'; Gotline
  (* backspace *)
  | 127 -> input_remove_char (); input_hide (); input_show (); Ok
  | _ -> input_add_char (Char.chr b); input_latest (); Ok

