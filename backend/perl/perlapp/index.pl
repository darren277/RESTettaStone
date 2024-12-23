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

$dbh = DBI->connect($connection_string, $username, $password, {AutoCommit => 0, RaiseError => 1, PrintError => 0});

sub handle_request {
    my $self = shift;
    my $cgi  = shift;

    my $path = $cgi->path_info();
    my $handler = $dispatch{$path};

    if (ref($handler) eq "CODE") {
        print "HTTP/1.0 200 OK\r\n";
        $handler->($cgi);

    } else {
        print "HTTP/1.0 404 Not found\r\n";
        print $cgi->header,
              $cgi->start_html('Not found'),
              $cgi->h1('Not found'),
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

sub to_json {
    my $data = shift;
    my $json = '[';
    my $first = 1;
    foreach my $row (@$data) {
        $json .= ',' if !$first;
        $json .= '{';
        my $first_field = 1;
        foreach my $field (keys %$row) {
            $json .= ',' if !$first_field;
            $json .= "\"$field\": \"$row->{$field}\"";
            $first_field = 0;
        }
        $json .= '}';
        $first = 0;
    }
    $json .= ']';
    return $json;
}

sub resp_users {
    print "Content-Type: application/json\n";
    print "Cache-Control: no-cache\n\n";

    my $sth = $dbh->prepare("SELECT * FROM users");
    $sth->execute();
    my $users = $sth->fetchall_arrayref({});
    print to_json($users);
}
}

# start the server on port 3012
#my $pid = MyWebServer->new(3012)->background();
#print "Use 'kill $pid' to stop server.\n";
#$pid->run();
#print "$pid is listening on port $ENV{PORT}\n";

my $pid = MyWebServer->new($ENV{PORT})->run();
