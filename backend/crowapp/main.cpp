// ADAPTED FROM: https://github.com/CarolCheng/lobby

#include "crow.h"
#include "pq_conn_pool.h"

struct userobj
{
    int id;
    std::string email;
};

int write_to_log(const std::string& msg)
{
    std::ofstream ofs("log.txt", std::ios::app);
    ofs << msg << std::endl;
    return 0;
};

int get_users(std::list<userobj> &lst_user)
{
	auto instance = pq_conn_pool::instance();
	auto dbconn = instance->burrow();
	std::string selectsql = "SELECT * FROM users";
	int nrow = -1;
	//write_to_log("about to select * from user table");
	try {
            pqxx::nontransaction work(*dbconn);
            try {
                pqxx::result res(work.exec(selectsql));
                try {
                    for (pqxx::result::const_iterator c = res.begin(); c != res.end(); ++c) {
                        userobj user1;
                        user1.id = c[0].as<int>();
                        user1.email = c[1].as<std::string>();
                        lst_user.push_back(user1);
                    }
		            nrow = res.size();
                }
                catch (const std::exception &ex) {
                    nrow = -1;
                    std::cout<< "Select failed: " << ex.what() << std::endl;
                    //write_to_log("Select failed: " + ex.what());
                    //write_to_log("Select failed.");
                }
            } catch (const std::exception &ex) {
                std::cout<< "Something in the middle failed: " << ex.what() << std::endl;
            }
        }
        catch (const std::exception &ex) {
            std::cout<< "Connection failed: " << ex.what() << std::endl;
        }
	instance->unburrow(dbconn);
	// return (nrow > 0) ? HTTP::to_uint(HTTPStatus::OK) : HTTP::to_uint(HTTPStatus::NotFound);
	return (nrow > 0) ? 200 : 404;
}

int main()
{
    std::string PORT_STRING = getEnvVar("CROWAPP_PORT");
    std::cout<< "PORT_STRING: " << PORT_STRING << std::endl;
    int PORT = std::stoi(PORT_STRING);

    crow::SimpleApp app;

    CROW_ROUTE(app, "/")([](){
        return "Hello world";
    });

    CROW_ROUTE(app, "/json")
    ([] {
        crow::json::wvalue x({{"zmessage", "Hello, World!"}, {"amessage", "Hello, World2!"}});
        return x;
    });

    CROW_ROUTE(app, "/api/users").methods("POST"_method, "GET"_method)
	([](const crow::request &req){
	    //write_to_log("about to process request");
	    //write_to_log("request method: " + req.method);
	    //write_to_log("request url: " + req.url);
	    //write_to_log("request body: " + req.body);
	    //write_to_log("request remote addr: " + req.remote_endpoint);
	    //write_to_log("request remote port: " + std::to_string(req.remote_port));

		if(req.method == "POST"_method) {
			auto msg = crow::json::load(req.body);
			if(!msg) return crow::response(400);
			userobj user11 = {-1, msg["email"].s()};
			//int response = add_user(user11);
			int response = 201;
			if(response != 201) return crow::response(response);
			crow::json::wvalue result = msg;
			result["id"] = user11.id;
			auto res = crow::response(response, result);
			res.set_header("Location", req.url + "/" + std::to_string(user11.id));
			return res;
		} else if(req.method == "GET"_method) {
			std::list<userobj> lst_users;
			int response = get_users(lst_users);
			if(response != 200) return crow::response(response);
			crow::json::wvalue result;
			unsigned int index = 0;
			for (auto iter = lst_users.begin(); iter != lst_users.end(); iter++, index++) {
				result[index]["id"] = iter->id;
				result[index]["email"] = iter->email;
			}
			return crow::response(response, result);
		}
		return crow::response(400);
	});

    app.port(PORT).multithreaded().run();
}
