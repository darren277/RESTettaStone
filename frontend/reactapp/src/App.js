import React from 'react';
import './App.css';
import {BrowserRouter as Router, Route, Switch} from 'react-router-dom'
import UsersTable from './components/UsersTable';
import Header from './components/Header';
import Footer from './components/Footer';

function App() {
  return (
    <div>
        <Router>
              <Header />
                <div className="container">
                    <Switch>
                          <Route path = "" exact component = {UsersTable}></Route>
                          <Route path = "users" component = {UsersTable}></Route>
                    </Switch>
                </div>
              <Footer />
        </Router>
    </div>

  );
}

export default App;
