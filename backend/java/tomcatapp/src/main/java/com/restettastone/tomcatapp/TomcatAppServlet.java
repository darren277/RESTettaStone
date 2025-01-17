package com.restettastone.tomcatapp;

import java.io.*;
import java.sql.*;
import javax.servlet.*;
import javax.servlet.http.*;
import org.json.JSONArray;
import org.json.JSONObject;

public class TomcatAppServlet extends HttpServlet {
    private static final String PG_HOST = System.getenv("PG_HOST") != null ? System.getenv("PG_HOST") : "localhost";
    private static final String PG_PORT = System.getenv("PG_PORT") != null ? System.getenv("PG_PORT") : "5432";
    private static final String PG_DB = System.getenv("PG_DB") != null ? System.getenv("PG_DB") : "postgres";
    private static final String PG_USER = System.getenv("PG_USER") != null ? System.getenv("PG_USER") : "postgres";
    private static final String PG_PASS = System.getenv("PG_PASS") != null ? System.getenv("PG_PASS") : "password";

    private static final String FILE_PATH = System.getenv("FILE_PATH") != null ? System.getenv("FILE_PATH") : "/tmp/file.txt";

    private String readRequestBody(HttpServletRequest request) throws IOException {
        StringBuilder stringBuilder = new StringBuilder();
        try (BufferedReader reader = request.getReader()) {
            String line;
            while ((line = reader.readLine()) != null) {
                stringBuilder.append(line);
            }
        }
        return stringBuilder.toString();
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        String path = request.getPathInfo();

        if ("/file".equals(path)) {
            handleFile(response);
        } else if ("/users".equals(path)) {
            handleGetUsers(response);
        } else if (path != null && path.matches("/users/\\d+")) {
            handleGetUserById(request, response);
        } else {
            response.setStatus(HttpServletResponse.SC_NOT_FOUND);
            response.getWriter().println("Endpoint not found");
        }
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        String path = request.getPathInfo();

        if ("/users".equals(path)) {
            handlePostUser(request, response);
        } else {
            response.setStatus(HttpServletResponse.SC_NOT_FOUND);
            response.getWriter().println("Endpoint not found");
        }
    }

    @Override
    protected void doPut(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        String path = request.getPathInfo();

        if (path != null && path.matches("/users/\\d+")) {
            handlePutUser(request, response);
        } else {
            response.setStatus(HttpServletResponse.SC_NOT_FOUND);
            response.getWriter().println("Endpoint not found");
        }
    }

    @Override
    protected void doDelete(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        String path = request.getPathInfo();

        if (path != null && path.matches("/users/\\d+")) {
            handleDeleteUser(request, response);
        } else {
            response.setStatus(HttpServletResponse.SC_NOT_FOUND);
            response.getWriter().println("Endpoint not found");
        }
    }

    private void handleFile(HttpServletResponse response) throws IOException {
        String absolutePath = getServletContext().getRealPath(FILE_PATH);

        File file = new File(absolutePath);
        if (!file.exists()) {
            response.setStatus(HttpServletResponse.SC_NOT_FOUND);
            response.getWriter().println("File not found");
            return;
        }

        response.setContentType("text/plain");

        try (FileInputStream fis = new FileInputStream(file);
             OutputStream os = response.getOutputStream()) {
            byte[] buffer = new byte[4096];
            int bytesRead;
            while ((bytesRead = fis.read(buffer)) != -1) {
                os.write(buffer, 0, bytesRead);
            }
        }
    }

    private void handleGetUsers(HttpServletResponse response) throws IOException {
        JSONArray usersList = new JSONArray();

        try {
            Class.forName("org.postgresql.Driver");
            String PG_URL = "jdbc:postgresql://" + PG_HOST + ":" + PG_PORT + "/" + PG_DB;
            try (Connection conn = DriverManager.getConnection(PG_URL, PG_USER, PG_PASS);
                 Statement stmt = conn.createStatement()) {

                String sql = "SELECT id, email FROM users";
                ResultSet rs = stmt.executeQuery(sql);

                while (rs.next()) {
                    JSONObject user = new JSONObject();
                    user.put("id", rs.getInt("id"));
                    user.put("email", rs.getString("email"));
                    usersList.put(user);
                }
                rs.close();
            }
        } catch (Exception e) {
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            response.getWriter().println("Database error: " + e.getMessage());
            return;
        }

        response.setContentType("application/json");
        response.getWriter().write(usersList.toString());
    }

    private void handlePostUser(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String requestBody = readRequestBody(request);
        JSONObject jsonObject = new JSONObject(requestBody);

        String email = jsonObject.optString("email", null);

        if (email == null) {
            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            response.getWriter().println("Email is required");
            return;
        }

        JSONObject user = new JSONObject();

        try {
            Class.forName("org.postgresql.Driver");
            String PG_URL = "jdbc:postgresql://" + PG_HOST + ":" + PG_PORT + "/" + PG_DB;
            try (Connection conn = DriverManager.getConnection(PG_URL, PG_USER, PG_PASS);
                 PreparedStatement stmt = conn.prepareStatement("INSERT INTO users (email) VALUES (?)")) {

                stmt.setString(1, email);
                stmt.executeUpdate();

                try (Statement stmt2 = conn.createStatement();
                     ResultSet rs = stmt2.executeQuery("SELECT id FROM users WHERE email = '" + email + "'")) {

                    if (rs.next()) {
                        user.put("id", rs.getInt("id"));
                        user.put("email", email);
                    }
                    rs.close();
                }
            }
        } catch (Exception e) {
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            response.getWriter().println("Database error: " + e.getMessage());
            return;
        }

        //response.setStatus(HttpServletResponse.SC_CREATED);
        response.setStatus(HttpServletResponse.SC_OK);
        response.setContentType("application/json");
        response.getWriter().write(user.toString());
    }

    private void handlePutUser(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String requestBody = readRequestBody(request);
        JSONObject jsonObject = new JSONObject(requestBody);

        String id = request.getPathInfo().split("/")[2];
        String email = jsonObject.optString("email", null);

        if (id == null || email == null) {
            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            response.getWriter().println("ID and email are required");
            return;
        }

        JSONObject user = new JSONObject();

        try {
            Class.forName("org.postgresql.Driver");
            String PG_URL = "jdbc:postgresql://" + PG_HOST + ":" + PG_PORT + "/" + PG_DB;
            try (
                Connection conn = DriverManager.getConnection(PG_URL, PG_USER, PG_PASS);
                PreparedStatement stmt = conn.prepareStatement("UPDATE users SET email = ? WHERE id = ? RETURNING id")
            ) {
                stmt.setString(1, email);
                stmt.setInt(2, Integer.parseInt(id));
                ResultSet rs = stmt.executeQuery();

                if (rs.next()) {
                    user.put("id", rs.getInt("id"));
                    user.put("email", email);
                } else {
                    response.setStatus(HttpServletResponse.SC_NOT_FOUND);
                    response.getWriter().println("User not found");
                    return;
                }
                rs.close();
            }
        } catch (Exception e) {
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            response.getWriter().println("Database error: " + e.getMessage());
            return;
        }

        response.setStatus(HttpServletResponse.SC_OK);
        response.setContentType("application/json");
        response.getWriter().write(user.toString());
    }

    private void handleDeleteUser(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String id = request.getPathInfo().split("/")[2];

        if (id == null) {
            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            response.getWriter().println("ID is required");
            return;
        }

        try {
            Class.forName("org.postgresql.Driver");
            String PG_URL = "jdbc:postgresql://" + PG_HOST + ":" + PG_PORT + "/" + PG_DB;
            try (Connection conn = DriverManager.getConnection(PG_URL, PG_USER, PG_PASS);
                 PreparedStatement stmt = conn.prepareStatement("DELETE FROM users WHERE id = ?")) {

                stmt.setInt(1, Integer.parseInt(id));
                stmt.executeUpdate();
            }
        } catch (Exception e) {
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            response.getWriter().println("Database error: " + e.getMessage());
            return;
        }

        response.setStatus(HttpServletResponse.SC_OK);
        response.getWriter().println("User deleted");
    }

    private void handleGetUserById(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String id = request.getPathInfo().split("/")[2];

        if (id == null) {
            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            response.getWriter().println("ID is required");
            return;
        }

        JSONObject user = new JSONObject();

        try {
            Class.forName("org.postgresql.Driver");
            String PG_URL = "jdbc:postgresql://" + PG_HOST + ":" + PG_PORT + "/" + PG_DB;
            try (Connection conn = DriverManager.getConnection(PG_URL, PG_USER, PG_PASS);
                 PreparedStatement stmt = conn.prepareStatement("SELECT id, email FROM users WHERE id = ?")) {

                stmt.setInt(1, Integer.parseInt(id));
                ResultSet rs = stmt.executeQuery();

                if (rs.next()) {
                    user.put("id", rs.getInt("id"));
                    user.put("email", rs.getString("email"));
                } else {
                    response.setStatus(HttpServletResponse.SC_NOT_FOUND);
                    response.getWriter().println("User not found");
                    return;
                }
                rs.close();
            }
        } catch (Exception e) {
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            response.getWriter().println("Database error: " + e.getMessage());
            return;
        }

        response.setContentType("application/json");
        response.getWriter().write(user.toString());
    }
}
