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
}
