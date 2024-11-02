import { Component, OnInit } from "@angular/core";
import { HttpClient } from "@angular/common/http";

import { environment } from '../environments/environment';


@Component({
  selector: "app-root",
  templateUrl: "./app.component.html",
  styleUrls: ["./app.component.css"],
})

export class AppComponent {
  title = "spMgAngDocker";

  constructor(private http: HttpClient) {}

  data = [];

  ngOnInit() {
    this.getData();
  }

  getData() {
    let url = `${environment.apiBaseUrl}/users`;
    this.http.get(url).subscribe((res) => {
      console.log(res);
      this.data = res as any[];
    });
  }

  addItem() {
    throw new Error('Not implemented');
    let url = "http://localhost:8080/addItem";
    let data = { itemName: "new item" };
    this.http
      .post(url, data, {
        headers: {
          "Content-Type": "application/json",
        },
      })
      .subscribe((res) => {
        console.log(res);
        this.getData();
      });
  }
}
