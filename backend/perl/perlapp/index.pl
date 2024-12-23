#!/usr/bin/perl
{
package MyWebServer;

use HTTP::Server::Simple::CGI;
use base qw(HTTP::Server::Simple::CGI);

my @routes = (
    { pattern => qr{^/users/\d+$}, handler => \&resp_user, is_regex => 1 },
    { pattern => '/users',         handler => \&resp_users, is_regex => 0 },
    { pattern => '/hello',         handler => \&resp_hello, is_regex => 0 },
);

use JSON;
use DBI;

my $username = $ENV{PG_USER};
my $password = $ENV{PG_PASS};

my $connection_string = "dbi:Pg:database=$ENV{PG_DB};host=$ENV{PG_HOST};port=$ENV{PG_PORT}";

my $dbh = DBI->connect($connection_string, $username, $password, {AutoCommit => 0, RaiseError => 1, PrintError => 0});

sub handle_request {
    my $self = shift;
    my $cgi  = shift;

    my $path = $cgi->path_info();
    my $handler;

    # Loop in a predictable order
    for my $route (@routes) {
        if ($route->{is_regex}) {
            if ($path =~ $route->{pattern}) {
                $handler = $route->{handler};
                last;
            }
        }
        else {
            if ($path eq $route->{pattern}) {
                $handler = $route->{handler};
                last;
            }
        }
    }

    if ($handler) {
        $handler->($cgi);
    }
    else {
        print "HTTP/1.0 404 Not Found\r\n";
        print $cgi->header,
              $cgi->start_html('Not Found'),
              $cgi->h1('Not Found'),
              $cgi->end_html;
    }
}

sub resp_hello {
    my $cgi  = shift;
    return if !ref $cgi;

    my $who = $cgi->param('name');

    print $cgi->header,
          $cgi->start_html("Hello"),
          $cgi->h1("Hello $who!"),
          $cgi->end_html;
}

# /users
sub resp_users {
    use CGI;
    my $cgi = CGI->new;

    #print "Cache-Control: no-cache\n\n";

    # check request method
    if ($ENV{REQUEST_METHOD} eq 'POST') {
        print STDERR "POST\n";
        print STDERR "Content-Type: " . $ENV{CONTENT_TYPE} . "\n";
        print STDERR "Content-Length: " . $ENV{CONTENT_LENGTH} . "\n";
        print STDERR "Request body: " . $cgi->param('POSTDATA') . "\n";
        # Request body: {"email": "test_email1@testing.com"}
        print STDERR "ENV: " . to_json(\%ENV) . "\n";

        my $post_data = $cgi->param('POSTDATA');
        my $json = from_json($post_data);
        my $email = $json->{email};

        unless ($email) {
            print "HTTP/1.0 404 Not Found\r\n";
            print "Content-Type: application/json\n\n";
            print to_json({ error => 'Email is required' });
            return;
        }

        # Log the email for debugging purposes
        print STDERR "email: $email\n";

        my $sth = $dbh->prepare("INSERT INTO users (email) VALUES (?)");
        $sth->execute($email);
        $dbh->commit();
        print "HTTP/1.0 200 OK\r\n";
        print "Content-Type: application/json\n\n";
        print to_json({ email => $email });
    }
    else { # Default to GET
        my $sth = $dbh->prepare("SELECT * FROM users");
        $sth->execute();
        my $users = $sth->fetchall_arrayref({});
        print "HTTP/1.0 200 OK\r\n";
        print "Content-Type: application/json\n\n";
        print to_json($users);
    }
}

# /users/id
sub resp_user {
    my $cgi = CGI->new;
    return if !ref $cgi;

    my ($id) = $cgi->path_info() =~ m!/users/(\d+)!;

    print STDERR "id: $id\n";

    if ($ENV{REQUEST_METHOD} eq 'PUT') {
        my $email = $cgi->param('email');

        my $sth = $dbh->prepare("SELECT * FROM users WHERE id = ?");
        $sth->execute($id);
        my $user = $sth->fetchrow_hashref();
        unless ($user) {
            print "HTTP/1.0 404 Not Found\r\n";
            print "Content-Type: application/json\n\n";
            print to_json({ error => 'User not found' });
            return;
        }

        $sth = $dbh->prepare("UPDATE users SET email = ? WHERE id = ?");
        $sth->execute($email, $id);
        $dbh->commit();
        print "HTTP/1.0 200 OK\r\n";
        print "Content-Type: application/json\n\n";
        print to_json({ id => $id, email => $email });
    }
    elsif ($ENV{REQUEST_METHOD} eq 'DELETE') {
        my $sth = $dbh->prepare("SELECT * FROM users WHERE id = ?");
        $sth->execute($id);
        my $user = $sth->fetchrow_hashref();
        unless ($user) {
            print "HTTP/1.0 404 Not Found\r\n";
            print "Content-Type: application/json\n\n";
            print to_json({ error => 'User not found' });
            return;
        }

        $sth = $dbh->prepare("DELETE FROM users WHERE id = ?");
        $sth->execute($id);
        $dbh->commit();
        print "HTTP/1.0 200 OK\r\n";
        print "Content-Type: application/json\n\n";
        print to_json({ id => $id });
    }
    elsif ($ENV{REQUEST_METHOD} eq 'GET') {
        my $sth = $dbh->prepare("SELECT * FROM users WHERE id = ?");
        $sth->execute($id);
        my $user = $sth->fetchrow_hashref();
        unless ($user) {
            print "HTTP/1.0 404 Not Found\r\n";
            print "Content-Type: application/json\n\n";
            print to_json({ error => 'User not found' });
            return;
        }
        print "HTTP/1.0 200 OK\r\n";
        print "Content-Type: application/json\n\n";
        print to_json($user);
    }
    else {
        print "HTTP/1.0 405 Method Not Allowed\r\n";
        print "Content-Type: application/json\n\n";
        print to_json({ error => 'Invalid request method' });
    }
}

}

# start the server on port 3012
#my $pid = MyWebServer->new(3012)->background();
#print "Use 'kill $pid' to stop server.\n";
#$pid->run();
#print "$pid is listening on port $ENV{PORT}\n";

my $pid = MyWebServer->new($ENV{PORT})->run();
