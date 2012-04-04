#include <iostream>
#include <sstream>
#include <stdexcept>
#include <postgresql/libpq-fe.h>

#define LINEN std::cerr << __FILE__ "(" << __LINE__ << "): warning "

struct SQLclient_Postgres {
    // see http://www.postgresql.org/docs/8.0/static/libpq-example.html

    // Internal exception which adds a Postgres error message.
    struct PGResultException : public std::runtime_error {
	PGResultException (std::string mesg, PGresult* res)
	    : std::runtime_error(mesg + PQresultErrorMessage(res))
	{
	    PQclear(res);
	} 
	virtual ~PGResultException () throw() {  }
    };

    struct Transaction {
	SQLclient_Postgres& client;
	int depth;
	bool committed;
	Transaction (SQLclient_Postgres& client)
	    : client(client), depth(client.transactionDepth), committed(false)
	{
	}
	~Transaction () {
	    if (client.transactionError)
		client._endTransaction(false);
	    else {
		if (client.transactionDepth != depth)
		    throw std::runtime_error
			("closing transactions out of order");
		client._endTransaction(committed);
	    }
	}
	void commit () {
	    if (committed)
		throw std::runtime_error("transaction already committed");
	    committed = true;
	}
    };

    PGconn* conn;
    int transactionDepth;
    bool transactionError;

    PGresult* Exec (PGconn* conn, const char* cmd) {
	std::cerr << "-- " << cmd << "\n";
	return PQexec(conn, cmd);
    }
    

    SQLclient_Postgres (std::string connStr)
	: conn(PQconnectdb(connStr.c_str())),
	  transactionDepth(0), transactionError(false)
    {
	if (PQstatus(conn) != CONNECTION_OK)
	    throw std::runtime_error(std::string("Connection failed: ")
				     + PQerrorMessage(conn));
    }
    virtual ~SQLclient_Postgres () {
	PQfinish(conn);
    }

    Transaction getTransaction () {
	if (transactionDepth == 0) {
	    // Start a transaction block.
	    PGresult* res = Exec(conn, "BEGIN");
	    if (PQresultStatus(res) != PGRES_COMMAND_OK)
		throw PGResultException("BEGIN command failed: ", res);
	    PQclear(res);
	}

	std::stringstream cmd;
	cmd << "SAVEPOINT s_" << transactionDepth+1;
	PGresult* res = Exec(conn, cmd.str().c_str());
	if (PQresultStatus(res) != PGRES_COMMAND_OK)
	    throw PGResultException("SAVEPOINT failed: ", res);
	PQclear(res);

	++transactionDepth;
	return Transaction(*this);
    }

    void _endTransaction (bool commit) {
	std::stringstream cmd;
	cmd << (commit ? "RELEASE" : "ROLLBACK TO")
	    << " SAVEPOINT s_" << transactionDepth;
	// LINEN << "cmd: " << cmd.str().c_str() << "\n";

	--transactionDepth;

	PGresult* res = Exec(conn, cmd.str().c_str());
	// LINEN << "post Exec\n";
	if (PQresultStatus(res) != PGRES_COMMAND_OK) {
	    transactionError = true; // error state for reaping Transactions
	    throw PGResultException("ending SAVEPOINT failed: ", res);
	}
	// LINEN << "post check\n";
	PQclear(res);
	// LINEN << "post clear\n";

	if (transactionDepth == 0) {
	    res = Exec(conn, transactionError ? "ROLLBACK" : "COMMIT");
	    PQclear(res);
	    transactionError = false;
	}
    }

    void execute (std::string cmd, bool useCursor = false) {

	PGresult *res;
	// Fetch rows, either via cursor or directly.
	if (useCursor) {

	    // Prepend cmd with a cursor named "theCursor".
	    cmd.insert(0, "DECLARE theCursor CURSOR FOR ");
	    res = Exec(conn, cmd.c_str());
	    if (PQresultStatus(res) != PGRES_COMMAND_OK)
		throw PGResultException("BDECLARE CURSOR failed: ", res);
	    PQclear(res);

	    // Get all the data from the cursor.
	    res = Exec(conn, "FETCH ALL IN theCursor");
	    if (PQresultStatus(res) != PGRES_TUPLES_OK && // SELECT
		PQresultStatus(res) != PGRES_COMMAND_OK)  // non-SELECT
		throw PGResultException("FETCH ALL failed: ", res);
	}
	else {

	    // Simply execute the command.
	    res = Exec(conn, cmd.c_str());
	    if (PQresultStatus(res) != PGRES_TUPLES_OK && // SELECT
		PQresultStatus(res) != PGRES_COMMAND_OK)  // non-SELECT
		throw PGResultException("EXEC failed: ", res);
	}

	if (PQresultStatus(res) == PGRES_TUPLES_OK) {
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
	}
	PQclear(res);

	/* Close the cursor. */
	if (useCursor) {
	    res = Exec(conn, "CLOSE theCursor");
	    PQclear(res);
	}
    }
};

int main(int argc, char *argv[]) {
	try {
	    SQLclient_Postgres pg("dbname = eric");
	    /* List the system catalog of databases (3x). */
	    pg.execute("SELECT * FROM pg_database");
	    SQLclient_Postgres::Transaction t3 = pg.getTransaction();
	    pg.execute("create table foo (id int);");
	    pg.execute("insert into foo (id) values (1);");
	    {
		SQLclient_Postgres::Transaction t1 = pg.getTransaction();
		pg.execute("insert into foo (id) values (2);");
		{
		    SQLclient_Postgres::Transaction t2 = pg.getTransaction();
		    pg.execute("insert into foo (id) values (3);");
		}
		t1.commit();
		pg.execute("insert into foo (id) values (4);");
		pg.execute("select * from foo;", true);
	    }
	    pg.execute("SELECT *\n FROM blah blah blah;"); // fails -- roll back to no table
	    pg.execute("drop table foo;"); // never reach here
	} catch (std::runtime_error& e) {
	    std::cerr << e.what();
	    return 222;
	}
	return 0;
    }
