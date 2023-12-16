let respond_peer_or_stdin (ic: in_channel) (oc: out_channel) = 
  let fd = Unix.descr_of_in_channel ic in
  let read_fs = [fd; Unix.stdin;] in
  let acknowledgement = "message received" in
  let sent_time = ref 0.0 in
  while true do
    let (ready, _, _) = Unix.select read_fs [] [] (-1.0) in
    List.iter 
      (fun ready -> 
        if ready = Unix.stdin then begin
          let line = read_line () ^ "\n" in
          output_string oc line;
          flush oc;
          sent_time := Sys.time ();
        end
        else if ready = fd then begin
          let line = input_line ic in
          if line = acknowledgement then begin
            let rtt: float = Float.min (Sys.time()) !sent_time in
            print_endline (acknowledgement ^ " rtt=" ^ Float.to_string rtt);
          end
          else begin
            let ack = acknowledgement ^ "\n" in
            output_string oc ack;
            flush oc;
            print_endline line
          end
        end
    ) ready;
  done;;

let sockaddr_to_string sockaddr = match sockaddr with
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (host, port) -> Unix.string_of_inet_addr host ^ string_of_int port

let client_handle_connection (ic: in_channel) (oc: out_channel) =
  let fd = Unix.descr_of_in_channel ic in
  let sockaddr = Unix.getsockname fd in
  print_endline ("[INFO] connection made to " ^ sockaddr_to_string sockaddr);
  try respond_peer_or_stdin ic oc with
   | End_of_file -> print_endline "[ERROR] server has closed the connection";
  close_out oc

let server_handle_connection (ic: in_channel) (oc: out_channel) =
  let fd = Unix.descr_of_in_channel ic in
  let sockaddr = Unix.getsockname fd in
  let client_addr = sockaddr_to_string sockaddr in
  print_endline ("[INFO] client connected from " ^ client_addr);
  try respond_peer_or_stdin ic oc with
   | End_of_file -> print_endline ("[INFO] client " ^ client_addr ^ " has disconnected");
  close_out oc

type mode = 
  | Client 
  | Server

let () = 
  let mode = ref Client in
  let set_mode (input: string) = 
    match input with
      | "client" -> mode := Client
      | "server" -> mode := Server
      | _ -> raise (Arg.Bad ("Input mode " ^ input ^ " is invalid!")) in
  let host = ref "" in
  let port = ref 0 in

  let usage_msg = "ocaml_chat [-mode] mode [-host] <host> [-port] <port>" in
  let spec = [
    ("-mode", Arg.String set_mode, "Run in server or client mode");
    ("-host", Arg.Set_string host, "Set host for server or client");
    ("-port", Arg.Set_int port, "Set port for server or client");
  ] in

  let do_nothing_with_string (_: string) : unit = () in

  let () = Arg.parse spec do_nothing_with_string usage_msg in

  let socket = Unix.socket PF_INET SOCK_STREAM 0 in
  let addr = try Unix.inet_addr_of_string !host with
    err -> 
      Printf.printf "[ERROR] failed to convert '%s' to inet_addr\n" !host;
      Unix.close socket; raise err in
  let sockaddr = Unix.ADDR_INET (addr, !port) in
  if !mode = Server then begin
    print_endline ("[INFO] starting server on " ^ sockaddr_to_string (sockaddr));
    Unix.establish_server server_handle_connection sockaddr
  end
  else if !mode = Client then begin
    let (ic, oc) = Unix.open_connection sockaddr in
    client_handle_connection ic oc
  end
