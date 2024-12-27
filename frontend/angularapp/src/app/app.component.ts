import { Component, OnInit } from "@angular/core";
import { HttpClient } from "@angular/common/http";

import { UserService } from "./user.service";


@Component({
    selector: "app-root",
    templateUrl: "./app.component.html",
    styleUrls: ["./app.component.css"],
})

export class AppComponent implements OnInit {
    title = "spMgAngDocker";
    data: any[] = [];
    newUser = { email: "" };
    editingUser: any = null;

    constructor(private userService: UserService) {}

    ngOnInit() {
        this.getAllUsers();
    }

    getAllUsers() {
        this.userService.getAllUsers().subscribe((res) => {
            this.data = res;
        });
    }

    getUserById(id: number) {
        this.userService.getUserById(id).subscribe((res) => {
            console.log(res);
        });
    }

    createUser() {
        if (!this.newUser.email) {
            alert("Email is required.");
            return;
        }

        this.userService.createUser(this.newUser).subscribe(() => {
            this.newUser.email = ""; // Reset the form
            this.getAllUsers(); // Refresh the list
        });
    }

    editUser(user: any) {
        this.editingUser = { ...user };
    }

    updateUser() {
        if (!this.editingUser || !this.editingUser.email) {
            alert("Email is required.");
            return;
        }

        this.userService.updateUser(this.editingUser.id, this.editingUser).subscribe(() => {
            this.editingUser = null; // Reset the form
            this.getAllUsers(); // Refresh the list
        });
    }

    deleteUser(id: number) {
        this.userService.deleteUser(id).subscribe(() => {
            this.getAllUsers(); // Refresh the list
        });
    }
}
