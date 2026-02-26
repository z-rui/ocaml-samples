module Unix = UnixLabels

let rec send_all conn buf pos len =
  if len > 0 then
    let n = Unix.send conn ~buf ~pos ~len ~mode:[] in
    if n >= 0 then send_all conn buf (pos + n) (len - n)

let rec handle_conn conn buf =
  let n = Unix.recv conn ~buf ~pos:0 ~len:(Bytes.length buf) ~mode:[] in
  if n > 0 then begin
    send_all conn buf 0 n;
    handle_conn conn buf
  end

let run_tcp_server listen_addr =
  let listener =
    Unix.socket ~cloexec:true ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0
  in
  Unix.bind listener ~addr:listen_addr;
  Unix.listen listener ~max:10;

  let readbuf = Bytes.create 4096 in
  while true do
    let conn, _ = Unix.accept listener in
    ignore
      (Thread.create
         begin fun conn ->
           Fun.protect ~finally:(fun () -> Unix.close conn) @@ fun () ->
           handle_conn conn readbuf
         end
         conn)
  done

let () =
  let port = 8086 in
  let listen_addr = Unix.inet_addr_loopback in
  Printf.printf "running echo server @ %s:%d\n%!"
    (Unix.string_of_inet_addr listen_addr)
    port;
  run_tcp_server (Unix.ADDR_INET (listen_addr, port))
