open Eio.Std

(* Eio makes it extremely easy to write an echo server *)
let handle_client flow peer_addr = Eio.Flow.copy flow flow

let main ~net ~addr =
  Switch.run ~name:"main" @@ fun sw ->
  let socket = Eio.Net.listen ~sw net addr ~backlog:10 in
  Eio.Net.run_server socket handle_client
    ~on_error:(traceln "Error handling connection: %a" Fmt.exn)

let () =
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8086) in
  Eio_main.run @@ fun env ->
  traceln "Running echo server @ %a" Eio.Net.Sockaddr.pp addr;
  main ~net:(Eio.Stdenv.net env) ~addr
