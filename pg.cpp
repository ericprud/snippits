#include <iostream>
#include <stdexcept>
#include <postgresql/libpq-fe.h>

struct SQLclient_Postgres {
    // see http://www.postgresql.org/docs/8.0/static/libpq-example.html

    typedef enum { TRANS_None = 0, TRANS_Transaction, TRANS_Cursor } e_TransactionMode;

    // Internal exception which adds a Postgres error message.
    struct PGResultException : public std::runtime_error {
	PGResultException (std::string mesg, PGresult* res)
	    : std::runtime_error(mesg + PQresultErrorMessage(res))
	{
	    PQclear(res);
	} 
	virtual ~PGResultException () throw() {  }
    };

    PGconn *conn;
    SQLclient_Postgres (std::string connStr)
	: conn(PQconnectdb(connStr.c_str()))
    {
	if (PQstatus(conn) != CONNECTION_OK)
	    throw std::runtime_error(std::string("Connection failed: ")
				     + PQerrorMessage(conn));
    }
    virtual ~SQLclient_Postgres () {
	PQfinish(conn);
    }

    struct Transaction {
	SQLclient_Postgres& client;
	Transaction (SQLclient_Postgres& client)
	    : client(client)
	{
	    // Start a transaction block.
	    PGresult* res = PQexec(client.conn, "BEGIN");
	    if (PQresultStatus(res) != PGRES_COMMAND_OK)
		throw PGResultException("BEGIN command failed: ", res);
	    PQclear(res);
	}
	~Transaction () {
	    PGresult* res = PQexec(client.conn, "END");
	    PQclear(res);
	}
    };

    void execute (std::string cmd, e_TransactionMode transactionMode = TRANS_None) {

	PGresult *res;
	// Fetch rows, either via cursor or directly.
	if (transactionMode == TRANS_Cursor) {

	    // Prepend cmd with a cursor named "theCursor".
	    cmd.insert(0, "DECLARE theCursor CURSOR FOR ");
	    res = PQexec(conn, cmd.c_str());
	    if (PQresultStatus(res) != PGRES_COMMAND_OK)
		throw PGResultException("BDECLARE CURSOR failed: ", res);
	    PQclear(res);

	    // Get all the data from the cursor.
	    res = PQexec(conn, "FETCH ALL IN theCursor");
	    if (PQresultStatus(res) != PGRES_TUPLES_OK)
		throw PGResultException("FETCH ALL failed: ", res);
	}
	else {

	    // Simply execute the command.
	    res = PQexec(conn, cmd.c_str());
	    if (PQresultStatus(res) != PGRES_TUPLES_OK)
		throw PGResultException("EXEC failed: ", res);
	}

	/* Print out the attribute names. */
	int nFields = PQnfields(res);
	for (int i = 0; i < nFields; i++)
	    printf("%-15s", PQfname(res, i));
	printf("\n");

	/* Print out row values. */
	for (int i = 0; i < PQntuples(res); i++) {
	    for (int j = 0; j < nFields; j++)
		printf("%-15s",PQgetvalue(res,i,j));
	    printf("\n");
	}
	printf("\n");
	PQclear(res);

	/* Close the cursor. */
	if (transactionMode == TRANS_Cursor) {
	    res = PQexec(conn, "CLOSE theCursor");
	    PQclear(res);
	}
    }
};

int main(int argc, char *argv[]) {
	try {
	    SQLclient_Postgres pg("dbname = eric");
	    /* List the system catalog of databases (3x). */
	    pg.execute("select * from pg_database");
	    {
		SQLclient_Postgres::Transaction trans(pg);
		pg.execute("select * from pg_database", SQLclient_Postgres::TRANS_Transaction);
		pg.execute("select * from pg_database", SQLclient_Postgres::TRANS_Cursor);
	    }
	    // pg.execute("create table foo (id int);");
	    // pg.execute("insert into foo (id) values (2);");
	    // pg.execute("insert into foo (id) values (1);");
	    // pg.execute("select * from foo;");
	    // pg.execute("drop table foo;");
	} catch (std::exception& e) {
	    std::cerr << e.what();
	    return 222;
	}
	return 0;
    }
