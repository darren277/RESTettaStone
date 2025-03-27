📦 sockets 🏠
📦 json 🏠

💭 Simple REST API server listening on port 3052
🏁 🍇
  💭 Create a server socket on port 3052
  🍺🆕🏄 3052❗ ➡️ server
  😀 🔤Server started on port 3052...🔤❗

  💭 Create a list of users for our demo API
  🆕🍨🐚🍯🐚🔡🍆🍆❗ ➡️ users

  💭 Create first user
  🆕🍯🐚🔡🍆❗ ➡️ user1
  🐻 user1 🔤id🔤 🔤1🔤❗
  🐻 user1 🔤name🔤 🔤John Doe🔤❗
  🐻 user1 🔤email🔤 🔤john@example.com🔤❗
  🐻 users user1❗

  💭 Create second user
  🆕🍯🐚🔡🍆❗ ➡️ user2
  🐻 user2 🔤id🔤 🔤2🔤❗
  🐻 user2 🔤name🔤 🔤Jane Smith🔤❗
  🐻 user2 🔤email🔤 🔤jane@example.com🔤❗
  🐻 users user2❗

  💭 Main server loop
  🔁 👍 🍇
    💭 Accept client connection
    🆗 clientSocket 🙋 server❗ 🍇
      💭 Process the client request
      processRequest clientSocket users❗
    🍉
    🙅‍♀️ error 🍇
      😀 🔤Error accepting connection: 🧲error🧲🔤❗
    🍉
  🍉
🍉

💭 Function to process HTTP requests
🐖 processRequest socket🍇 users🍨🐚🍯🐚🔡🍆🍆 🍇
  💭 Read the HTTP request
  🆗 requestData 👂 socket 1024❗ 🍇
    🔡 requestData❗ ➡️ request

    💭 Parse the request line to get method and path
    🔫 request 🔤\n🔤❗ ➡️ lines
    🐽 lines 0❗ ➡️ requestLine
    🔫 requestLine 🔤 🔤❗ ➡️ requestParts

    🐽 requestParts 0❗ ➡️ method
    🐽 requestParts 1❗ ➡️ path

    😀 🔤Received 🧲method🧲 request for 🧲path🧲🔤❗

    💭 Handle different routes
    ↪️ 🐔 path 🔤/users🔤❗ 🍇
      💭 Respond with the users list as JSON
      🆕🔷🕊📄🦘 users❗ ➡️ usersJson
      🍺 🔡 usersJson❗ ➡️ usersJsonString
      respondWithJson socket usersJsonString❗
    🍉
    🙅‍♀️ 🍇
      💭 Respond with 404 Not Found for other routes
      🔤404 Not Found🔤 ➡️ notFoundMessage
      respondWithStatus socket 🔤404 Not Found🔤 notFoundMessage❗
    🍉
  🍉
  🙅‍♀️ error 🍇
    😀 🔤Error reading request: 🧲error🧲🔤❗
  🍉

  💭 Close the connection
  🙅 socket❗
🍉

💭 Function to send a JSON response
🐖 respondWithJson socket🍇 jsonContent 🔡 🍇
  🔤HTTP/1.1 200 OK
Content-Type: application/json
Content-Length: 🧲🔡 📏 jsonContent❗❗🧲

🧲jsonContent🧲🔤 ➡️ response

  💬 socket 🚂 response❗❗
🍉

💭 Function to send a status response
🐖 respondWithStatus socket🍇 status 🔡 message 🔡 🍇
  🔤HTTP/1.1 🧲status🧲
Content-Type: text/plain
Content-Length: 🧲🔡 📏 message❗❗🧲

🧲message🧲🔤 ➡️ response

  💬 socket 🚂 response❗❗
🍉
