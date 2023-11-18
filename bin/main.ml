let ipv4_address = Unix.inet_addr_any

let server_port = 8080

let protocol_version = "HTTP/1.1"
let end_start_line = "\r\n"
let end_headers = "\r\n\r\n"


type http_method = METHOD of string | METHOD_OTHER (*| POST | UPDATE | PUT | DELETE*)
type http_status_code = STATUS of int

let get_sock_addr (addr, port) = Unix.ADDR_INET (addr,port)

let get_client_ip client_addr = match client_addr with
	| Unix.ADDR_UNIX addr -> addr
	| Unix.ADDR_INET (addr, _) -> Unix.string_of_inet_addr addr

let rotate_right fn b c a = fn a b c

let get_tcp_socket addr port = 
	get_sock_addr (addr, port)
	|> Unix.domain_of_sockaddr
	|> rotate_right Unix.socket Unix.SOCK_STREAM 0


let number_of_waiting_requests = 5

let socket_fd = get_tcp_socket ipv4_address server_port


let get_http_method req = match List.hd (String.split_on_char '\n' req) with
		| x when String.starts_with ~prefix:"GET" x  -> METHOD "GET"
		| _ -> METHOD_OTHER

let get_status_code_string status = match status with
	| STATUS x when x = 200 -> "200 OK"
	| _ -> "200 OK"


let ( ^^ ) a b = a ^ " " ^ b


let create_response () = 
	let body = "<html><body><h1>Hello from Ocaml!</h1></body></html>" in
	let response = 
		protocol_version ^^ get_status_code_string (STATUS 200) ^ 
		end_start_line ^
		"Connection: Close\r\n" ^
		"Content-Type: text/html; charset=utf-8\r\n" ^
		Format.sprintf "Content-Length: %d" (Bytes.length (String.to_bytes body)) ^ (* The number of bytes of the body *)
		end_headers ^
		body
	in
		Bytes.of_string response

let handle_request req fd =
	(* Format.printf "%s" req; *)
	let _ = get_http_method req in
	let response = create_response () in
	let bytes_sent = Unix.send fd response 0 (Bytes.length response) [] in
	(* Close connection *)
	Format.printf "Sent %d bytes.\n" bytes_sent;
	Unix.shutdown fd Unix.SHUTDOWN_ALL;
	Unix.close fd

let open_socket () = 
		Unix.bind socket_fd (get_sock_addr (ipv4_address, server_port));  
		Unix.listen socket_fd number_of_waiting_requests; Format.printf "Listening for requests on %d \n" server_port

let serve () = 
	let (response_socket_fd, client_addr) = 
		print_endline "waiting for connection"; 
	Unix.accept socket_fd in
	let (buffer_start, buffer_size) = (0, 65535) in
	let buffer = Bytes.create buffer_size in
		let bytes_received = 
				Format.printf "Incoming request from %s: \n" (get_client_ip client_addr);
				Unix.recv response_socket_fd buffer buffer_start buffer_size [] 
		in let request = Bytes.sub buffer 0 bytes_received in
			handle_request (Bytes.to_string request) response_socket_fd



let close () = Unix.close socket_fd

let () = 
	open_socket ();
	serve ();
	close ();