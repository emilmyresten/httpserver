
let protocol_version = "HTTP/1.1"
let end_start_line = "\r\n"
let end_headers = "\r\n\r\n"


type http_method = METHOD of string | METHOD_OTHER (*| POST | UPDATE | PUT | DELETE*)
type http_status_code = STATUS of int

let get_sock_addr (addr, port) = Unix.ADDR_INET (addr,port)

let get_client_ip client_addr = match client_addr with
	| Unix.ADDR_UNIX addr -> addr
	| Unix.ADDR_INET (addr, _) -> Unix.string_of_inet_addr addr

let get_socket_port sockaddr = match sockaddr with
	| Unix.ADDR_UNIX _ -> 0
	| Unix.ADDR_INET (_, port) -> int_of_string (Printf.sprintf "%d" port)

let rotate_right fn b c a = fn a b c

let get_tcp_socket addr port = 
	get_sock_addr (addr, port)
	|> Unix.domain_of_sockaddr
	|> rotate_right Unix.socket Unix.SOCK_STREAM 0


let number_of_waiting_requests = 5


let get_http_method req = match List.hd (String.split_on_char '\n' req) with
		| x when String.starts_with ~prefix:"GET" x  -> METHOD "GET"
		| _ -> METHOD_OTHER

let get_status_code_string status = match status with
	| STATUS x when x = 200 -> "200 OK"
	| _ -> "200 OK"


let ( ^^ ) a b = a ^ " " ^ b

let get_file_content file = 
	let ic = Stdlib.open_in file in
	let rec consume acc =
		try
			consume (acc ^ Stdlib.input_line ic)
		with _ ->
			Stdlib.close_in ic;
			acc
		in 
		consume ""

let create_response html = 
	let body = get_file_content html in (* Read and serve index.html! *)
	let response = 
		protocol_version ^^ get_status_code_string (STATUS 200) ^ 
		end_start_line ^
		"Connection: Close\r\n" ^
		"Content-Type: text/html; charset=utf-8\r\n" ^
		Printf.sprintf "Content-Length: %d" (Bytes.length (String.to_bytes body)) ^ (* The number of bytes of the body *)
		end_headers ^
		body
	in
		Bytes.of_string response

let handle_request req fd html =
	let _ = get_http_method req in
	let response = create_response html in
	let bytes_sent = Unix.send fd response 0 (Bytes.length response) [] in
		(* Close connection *)
		Printf.printf "Sent %d bytes.\n" bytes_sent;
		Stdlib.flush Stdlib.stdout;
		Unix.shutdown fd Unix.SHUTDOWN_ALL;
		Unix.close fd

let open_socket port = 
	let ipaddr = Unix.inet_addr_any in
	let socket_fd = get_tcp_socket ipaddr port in
		Unix.bind socket_fd (get_sock_addr (ipaddr, port));  
		Unix.listen socket_fd number_of_waiting_requests; 
		socket_fd

(* let close socket_fd = 
	Unix.close socket_fd;
	Printf.printf "Closed socket.\n";
	Stdlib.flush Stdlib.stdout *)

	

let rec serve_loop socket_fd html = 
	let (response_socket_fd, client_addr) = 
		Printf.printf "waiting for connection on %d\n" (get_socket_port (Unix.getsockname socket_fd));
		Stdlib.flush Stdlib.stdout;
		Unix.accept socket_fd in (* the handling of request should be threaded out, and the main thread should immediately return to block on accept. *)
	let (buffer_start, buffer_size) = (0, 65535) in
	let buffer = Bytes.create buffer_size in
	let bytes_received = 
		Printf.printf "Incoming request from %s: \n" (get_client_ip client_addr);
		Stdlib.flush Stdlib.stdout;
		Unix.recv response_socket_fd buffer buffer_start buffer_size [] 
	in let request = Bytes.sub buffer 0 bytes_received in
		handle_request (Bytes.to_string request) response_socket_fd html;
		serve_loop socket_fd html
		(* close socket_fd; *)


let serve port html =
	let socket_fd = open_socket port in
	serve_loop socket_fd html
		(* close response_socket_fd; *)
