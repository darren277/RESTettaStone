import { Injectable } from "@angular/core";
import { HttpClient } from "@angular/common/http";
import { Observable } from "rxjs";
import { environment } from '../environments/environment';

@Injectable({
    providedIn: "root",
})
export class UserService {
    private apiUrl = `${environment.apiBaseUrl}/users`;

    constructor(private http: HttpClient) {}

    getAllUsers(): Observable<any[]> {
        return this.http.get<any[]>(this.apiUrl);
    }

    getUserById(id: number): Observable<any> {
        return this.http.get<any>(`${this.apiUrl}/${id}`);
    }

    createUser(user: { email: string }): Observable<any> {
        return this.http.post<any>(this.apiUrl, user);
    }

    updateUser(id: number, user: { email: string }): Observable<any> {
        return this.http.put<any>(`${this.apiUrl}/${id}`, user);
    }

    deleteUser(id: number): Observable<any> {
        return this.http.delete<any>(`${this.apiUrl}/${id}`);
    }
}
