import React from 'react';
import './App.css';
import {BrowserRouter as Router, Route, Switch} from 'react-router-dom'
import ListUsers from './components/ListUsers';
import Header from './components/Header';
import Footer from './components/Footer';
//import CreateUser from './components/CreateUser';
//import UpdateUser from './components/UpdateUser';
//import ViewUser from './components/ViewUser';

function App() {
  return (
    <div>
        <Router>
              <Header />
                <div className="container">
                    <Switch>
                          <Route path = "" exact component = {ListUsers}></Route>
                          <Route path = "users" component = {ListUsers}></Route>
                          {/* <Route path = "/add-user/:id" component = {CreateUser}></Route> */}
                          {/* <Route path = "/view-user/:id" component = {ViewUser}></Route> */}
                          {/* <Route path = "/update-user/:id" component = {UpdateUser}></Route> */}
                    </Switch>
                </div>
              <Footer />
        </Router>
    </div>

  );
}

export default App;
