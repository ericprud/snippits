/*
  SPARQLalgebra - test of SPARQL definition of PropertyPath evaluation.

  http://www.w3.org/TR/sparql11-query/#PropertyPathPatterns

  g++ -g -o SPARQLalgebra SPARQLalgebra.cpp -Wall -Werror -std=c++0x && valgrind ./SPARQLalgebra
 */

#include <iostream>
#include <list>
#include <map>
#include <set>
#include <cassert>
#include <functional>

#define QUOTE // "\"" // uncomment to print pastable C code.

namespace result { struct Solution; } // forward decl for TermOrVar::matches


namespace term {

    // Definition: RDF Term
    // http://www.w3.org/2009/sparql/docs/query-1.1/rq25.xml#defn_RDFTerm
    struct Term {
	enum class Type { I, L, B };
	Type t;
	std::string lexicalForm;

	Term (Type t, std::string lexicalForm)
	    : t(t), lexicalForm(lexicalForm) { check(); }

	void check () {
	    assert(t == Type::I || t == Type::L || t == Type::B);
	}

	bool operator== (const Term& r) const {
	    return
		Term::t == r.Term::t && lexicalForm == r.lexicalForm;
	}

	bool operator< (const Term& r) const {
	    return
		Term::t == r.Term::t
		? lexicalForm < r.lexicalForm
		: Term::t < r.Term::t;
	}

	std::ostream& print (std::ostream& os) const {
	    switch (Term::t) {
	    case Term::Type::I: return os << "I("QUOTE << lexicalForm << QUOTE")";
	    case Term::Type::L: return os << "L("QUOTE << lexicalForm << QUOTE")";
	    case Term::Type::B: return os << "B("QUOTE << lexicalForm << QUOTE")";
	    }
	    assert(false);
	}
    };
    std::ostream& operator<< (std::ostream& os, const Term& ref) {
	return ref.print(os);
    }

    struct I : Term {
	I (std::string lexicalForm)
	    : Term (Term::Type::I, lexicalForm) {  }
    };

    struct L : Term {
	L (std::string lexicalForm)
	    : Term (Term::Type::L, lexicalForm) {  }
    };

    struct B : Term {
	B (std::string lexicalForm)
	    : Term (Term::Type::B, lexicalForm) {  }
    };

    // Definition: Query Variable
    // http://www.w3.org/2009/sparql/docs/query-1.1/rq25.xml#defn_QueryVariable
    struct Var {
	std::string lexicalForm;
	Var (std::string lexicalForm)
	    : lexicalForm(lexicalForm) {  }
	bool operator== (const Var& r) const {
	    return lexicalForm == r.lexicalForm;
	}
	bool operator< (const Var& r) const {
	    return lexicalForm < r.lexicalForm;
	}
	std::ostream& print (std::ostream& os) const {
	    os << "Var("QUOTE << lexicalForm << QUOTE")";
	    return os;
	}
    };
    std::ostream& operator<< (std::ostream& os, const Var& ref) {
	return ref.print(os);
    }

    struct TermOrVar {
	enum class Type { Term, Var };
	Type t;
	Term term;
	Var var;
	TermOrVar (Term term)
	    : t(Type::Term), term(term), var(Var("!")) { check(); }
	TermOrVar (Var var)
	    : t(Type::Var), term(I("!")), var(var) { check(); }
	void check () {
	    assert(t == Type::Term || t == Type::Var);
	}
	bool operator== (const TermOrVar& r) const {
	    switch (t) {
	    case Type::Term: return r.t == Type::Term && term == r.term;
	    case Type::Var:  return r.t == Type::Var  && var  == r.var;
	    default: assert(false);
	    }
	}
	bool operator< (const TermOrVar& r) const {
	    switch (t) {
	    case Type::Term: {
		return
		    r.t == Type::Term
		    ? term < r.term
		    : r.t < Type::Term;
	    }
	    case Type::Var: {
		return
		    r.t == Type::Var
		    ? var  < r.var
		    : r.t < Type::Var;
	    }
	    default: assert(false);
	    }
	}
	std::ostream& print (std::ostream& os) const {
	    switch (t) {
	    case Type::Term:  return os << term;
	    case Type::Var:   return os << var;
	    }
	    assert(false);
	}
    };

    std::ostream& operator<< (std::ostream& os, const TermOrVar& tov) {
	return tov.print(os);
    }

    namespace test {
	void All () {
	    I i1("iri-"), i2("iri-"), i3("iri3");
	    // std::cout << i1 << (i1 == i2 ? "==" : "!=") << i2 << std::endl;
	    // std::cout << i1 << (i1 == i3 ? "==" : "!=") << i3 << std::endl;
	    assert(i1 == i2);
	    assert(!(i1 == i3));
	}
    };

} // namespace term;


namespace graph {
    struct Triple {
	term::Term s, p, o;
	Triple (term::Term s, term::Term p, term::Term o)
	    : s(s), p(p), o(o) {  }
	bool operator< (const Triple& r) const {
	    return
		!(s == r.s) ? s < r.s :
		!(p == r.p) ? p < r.p :
		o < r.o;
	}
	std::ostream& print (std::ostream& os) const {
	    return os << "Triple(" << s << " " << p << " " << o << ")";
	}
    };
    std::ostream& operator<< (std::ostream& os, const Triple& ref) {
	return ref.print(os);
    }

    struct Graph : std::set<Triple> {
	Graph (std::initializer_list<Triple> l) : std::set<Triple>(l) {  }

	std::ostream& print (std::ostream& os) const {
	    for (iterator it = begin(); it != end(); ++it) {
		if (it != begin())
		    os << "\n";
		it->print(os);
	    }
	    return os;
	}
    };
    std::ostream& operator<< (std::ostream& os, const Graph& ref) {
	return ref.print(os);
    }

    // Definition: Triple Pattern
    // http://www.w3.org/2009/sparql/docs/query-1.1/rq25.xml#defn_TriplePattern
    struct TriplePattern {
	term::TermOrVar s, p, o;
	TriplePattern (term::TermOrVar s, term::TermOrVar p, term::TermOrVar o)
	    : s(s), p(p), o(o) {  }
	bool operator< (const TriplePattern& r) const {
	    return
		!(s == r.s) ? s < r.s :
		!(p == r.p) ? p < r.p :
		o < r.o;
	}
	std::ostream& print (std::ostream& os) const {
	    return os << "TriplePattern(" << s << " " << p << " " << o << ")";
	}
    };
    std::ostream& operator<< (std::ostream& os, const TriplePattern& ref) {
	return ref.print(os);
    }

    // Definition: Basic Graph Pattern
    // http://www.w3.org/2009/sparql/docs/query-1.1/rq25.xml#defn_BasicGraphPattern
    struct BGP : std::set<TriplePattern> {
	BGP (std::initializer_list<TriplePattern> l) : std::set<TriplePattern>(l) {  }

	std::ostream& print (std::ostream& os) const {
	    for (iterator it = begin(); it != end(); ++it) {
		if (it != begin())
		    os << "\n";
		it->print(os);
	    }
	    return os;
	}
    };
    std::ostream& operator<< (std::ostream& os, const BGP& ref) {
	return ref.print(os);
    }
} // namespace graph


namespace result {

    // μ
    typedef std::map<term::Var, term::Term> SolutionContainer;
    struct Solution : SolutionContainer {
	Solution (std::initializer_list<SolutionContainer::value_type> l)
	    : SolutionContainer(l) {  }
	std::ostream& print (std::ostream& os) const {
	    for (const_iterator it = begin(); it != end(); ++it) {
		if (it != begin())
		    os << ", ";
		it->first.print(os);
		os << "=";
		it->second.print(os);		
	    }
	    return os;
	}

	// Write μ0 for the mapping such that dom(μ0) is the empty set.
	// http://www.w3.org/TR/sparql11-query/#BasicGraphPattern
	const static result::Solution Solution0;
    };
    const Solution Solution::Solution0 = Solution{};
    std::ostream& operator<< (std::ostream& os, const Solution& ref) {
	return ref.print(os);
    }

    // Ω
    struct Multiset : std::list<Solution> {
	Multiset (std::initializer_list<Solution> i)
	    : std::list<Solution>(i) {  }
	std::ostream& print (std::ostream& os) const {
	    for (const_iterator it = begin(); it != end(); ++it) {
		if (it != begin())
		    os << "\n";
		it->print(os);
	    }
	    return os;
	}

	// Write Ω0 for the multiset consisting of exactly the empty mapping μ0, with cardinality 1.
	// http://www.w3.org/TR/sparql11-query/#BasicGraphPattern
	const static result::Multiset Multiset0;
    };
    const Multiset Multiset::Multiset0 = Multiset{Solution::Solution0};
    std::ostream& operator<< (std::ostream& os, const Multiset& ref) {
	return ref.print(os);
    }
} // namespace result


namespace path {
    struct PathElt {
	enum class Type {
	    link, inv, NPS, seq, alt, ZeroOrMorePath, OneOrMorePath, ZeroOrOnePath
	};
	Type t;
	PathElt (Type t)
	    : t(t) { check(); }
	void check () {
	    assert(t == Type::link || t == Type::inv || t == Type::NPS ||
		   t == Type::seq || t == Type::alt || t == Type::ZeroOrMorePath
		   || t == Type::OneOrMorePath || t == Type::ZeroOrOnePath);
	}
    };

    struct link : PathElt {
	const term::I& i;
	link (const term::I& i)
	    : PathElt(PathElt::Type::link), i(i) {  }
    };

    struct inv : PathElt {
	const PathElt& p;
	inv (const PathElt& p)
	    : PathElt(PathElt::Type::inv), p(p) {  }
    };

    struct NPS : PathElt {
    	std::set<std::reference_wrapper<const term::I> > s;
    	NPS (std::set<std::reference_wrapper<const term::I> > s)
	    : PathElt(PathElt::Type::NPS), s(s) {  }
    };

    struct seq : PathElt {
	const PathElt& l;
	const PathElt& r;
	seq (const PathElt& l, const PathElt& r)
	    : PathElt(PathElt::Type::seq), l(l), r(r) {  }
    };

    struct alt : PathElt {
	const PathElt& l;
	const PathElt& r;
	alt (const PathElt& l, const PathElt& r)
	    : PathElt(PathElt::Type::alt), l(l), r(r) {  }
    };

    struct ZeroOrMorePath : PathElt {
	const PathElt& p;
	ZeroOrMorePath (const PathElt& p)
	    : PathElt(PathElt::Type::ZeroOrMorePath), p(p) {  }
    };

    struct OneOrMorePath : PathElt {
	const PathElt& p;
	OneOrMorePath (const PathElt& p)
	    : PathElt(PathElt::Type::OneOrMorePath), p(p) {  }
    };

    struct ZeroOrOnePath : PathElt {
	const PathElt& p;
	ZeroOrOnePath (const PathElt& p)
	    : PathElt(PathElt::Type::ZeroOrOnePath), p(p) {  }
    };

    struct Path {
	term::TermOrVar s;
	PathElt p;
	term::TermOrVar o;
	Path (term::TermOrVar s, PathElt p, term::TermOrVar o)
	    : s(s), p(p), o(o) {  }
    };
} // namespace path;


namespace eval {
    result::Multiset eval (path::Path path) {
	result::Multiset ret = result::Multiset::Multiset0; // 1 row, no bindings
	return ret;
    }

    // Definition: Basic Graph Pattern Matching
    // http://www.w3.org/TR/sparql11-query/#BGPsparql
    result::Multiset BasicGraphPatternMatching (graph::BGP& BGP, graph::Graph& G) {

	/* term::VarOrBNode captures the types serving as variables in a Solution.
	   Including BNodes allows one algorithm to handle both variable binding
	   and BNode-isomorphism.
	*/
	struct VarOrBNode {
	    enum class Type { Var, BNode };
	    Type t;
	    term::Var var;
	    term::B bnode;

	    VarOrBNode (term::Var var)
		: t(Type::Var), var(var), bnode(term::B("!")) { check(); }
	    VarOrBNode (term::B bnode)
		: t(Type::BNode), var(term::Var("!")), bnode(bnode) { check(); }
	    void check () {
		assert(t == Type::Var || t == Type::BNode);
	    }
	    bool operator== (const VarOrBNode& r) const {
		switch (t) {
		case Type::Var:   return r.t == Type::Var   && var   == r.var;
		case Type::BNode: return r.t == Type::BNode && bnode == r.bnode;
		default: assert(false);
		}
	    }
	    bool operator< (const VarOrBNode& r) const {
		switch (t) {
		case Type::Var: {
		    return
			r.t == Type::Var
			? var < r.var
			: r.t < Type::Var;
		}
		case Type::BNode: {
		    return
			r.t == Type::BNode
			? bnode < r.bnode
			: r.t < Type::BNode;
		}
		default: assert(false);																}
	    }
	    std::ostream& print (std::ostream& os) const {
		switch (t) {
		case Type::Var:   return os << var;
		case Type::BNode: return os << bnode;
		}
		assert(false);
	    }
	};

	// Solution - same as result::Solution except it permits BNodes.
	typedef std::map<VarOrBNode, term::Term> SolutionContainer;
        struct Solution : SolutionContainer {
	    Solution (std::initializer_list<SolutionContainer::value_type> l)
		: SolutionContainer(l) {  }
	    std::ostream& print (std::ostream& os) const {
		for (const_iterator it = begin(); it != end(); ++it) {
		    if (it != begin())
			os << ", ";
		    it->first.print(os);
		    os << "=";
		    it->second.print(os);		
		}
		return os;
	    }

	    // additional functions to support BGP matching.
	    bool freeOrEquals (const VarOrBNode v, const term::Term k) {
		iterator it = find(v);
		if (it == end()) {
		    insert(std::make_pair(v, k));
		    return true;
		}
		return it->second == k;
	    }
	    bool matches (const term::TermOrVar fromTP, const term::Term& fromT) {
		switch (fromTP.t) {
		case term::TermOrVar::Type::Term: {
		    switch (fromTP.term.t) {
		    case term::Term::Type::I: return fromTP.term == fromT;
		    case term::Term::Type::L: return fromTP.term == fromT;
		    case term::Term::Type::B:
			term::B b(fromTP.term.lexicalForm); // @@ better idea?
			return freeOrEquals(VarOrBNode(b), fromT);
		    }
		}
		case term::TermOrVar::Type::Var:
		    return freeOrEquals(VarOrBNode(fromTP.var), fromT);
		}
		assert(false);
	    }
	};

	// Multiset - same as result::Multiset except it permits BNodes.
	struct Multiset : std::list<Solution> {
	    Multiset (std::initializer_list<Solution> i)
		: std::list<Solution>(i) {  }
	    std::ostream& print (std::ostream& os) const {
		for (const_iterator it = begin(); it != end(); ++it) {
		    if (it != begin())
			os << "\n";
		    it->print(os);
		}
		return os;
	    }
	};

	Multiset bindings{Solution{}}; // 1 row, 0 var/bnode bindings.

	// Explore permutations of the graph, populating var/bnode bindings.
	for (graph::BGP::const_iterator tp = BGP.begin(); tp != BGP.end(); ++tp) {
	    for (Multiset::iterator sit = bindings.begin(); sit != bindings.end(); ) {
		Solution s = *sit;
		sit = bindings.erase(sit);
		for (graph::Graph::const_iterator t = G.begin(); t != G.end(); ++t) {
		    Solution s2 = s;
		    if (s2.matches(tp->s, t->s) &&
			s2.matches(tp->p, t->p) &&
			s2.matches(tp->o, t->o))
			bindings.insert(sit, s2);
		}
	    }
	}

	// Return only the Var bindings (remove BNode mappings).
	result::Multiset ret{};
	for (Multiset::const_iterator row = bindings.begin(); row != bindings.end(); ++row) {
	    result::Solution s {};
	    for (Solution::const_iterator col = row->begin(); col != row->end(); ++col) {
		if (col->first.t == VarOrBNode::Type::Var)
		    s.insert(std::make_pair(col->first.var, col->second));
	    }
	    ret.insert(ret.end(), s);
	} 

	return ret;
    }

    namespace test {
	void All () {
	    graph::Graph g {
		graph::Triple(term::I("N1"), term::I("P1"), term::I("N2")),
		graph::Triple(term::I("N2"), term::I("P2"), term::L("L3")),
		graph::Triple(term::I("N2"), term::I("P3"), term::L("L3"))
	    };
	    // std::cout << g << "\n";

	    graph::BGP bgp {
		graph::TriplePattern(term::I("N1"), term::Var("px"), term::B("n2")),
		graph::TriplePattern(term::B("n2"), term::Var("py"), term::L("L3"))
	    };
	    // std::cout << bgp << "\n";

	    result::Multiset r = eval::BasicGraphPatternMatching(bgp, g);
	    // std::cout << r << "\n";

	    result::Multiset expected {
		result::Solution {{term::Var("px"), term::I("P1")}, {term::Var("py"), term::I("P2")}},
		result::Solution {{term::Var("px"), term::I("P1")}, {term::Var("py"), term::I("P3")}}
	    };
	    // std::cout << expected << "\n";
	    assert(r == expected);

	    result::Multiset notExpected_var {
		result::Solution {{term::Var("px999"), term::I("P1")}, {term::Var("py"), term::I("P2")}},
		result::Solution {{term::Var("px"),    term::I("P1")}, {term::Var("py"), term::I("P3")}}
	    };
	    assert(r != notExpected_var);

	    result::Multiset notExpected_val {
		result::Solution {{term::Var("px"), term::I("P1999")}, {term::Var("py"), term::I("P2")}},
		result::Solution {{term::Var("px"), term::I("P1")},    {term::Var("py"), term::I("P3")}}
	    };
	    assert(r != notExpected_val);

	    result::Multiset notExpected_slot {
		result::Solution {{term::Var("px"), term::I("P1")}, {term::Var("py"), term::I("P2")}, {term::Var("pz"), term::I("P2")}},
		result::Solution {{term::Var("px"), term::I("P1")}, {term::Var("py"), term::I("P3")}}
	    };
	    assert(r != notExpected_slot);

	    result::Multiset notExpected_row {
		result::Solution {{term::Var("px"), term::I("P1")}, {term::Var("py"), term::I("P2")}},
		result::Solution {{term::Var("px"), term::I("P1")}, {term::Var("py"), term::I("P2")}},
		result::Solution {{term::Var("px"), term::I("P1")}, {term::Var("py"), term::I("P3")}}
	    };
	    assert(r != notExpected_row);

	    result::Multiset notExpected_noSlot {
		result::Solution {{term::Var("px"), term::I("P1")}},
		result::Solution {{term::Var("px"), term::I("P1")}}
	    };
	    assert(r != notExpected_noSlot);

	    result::Multiset notExpected_noRow {
		result::Solution {{term::Var("px"), term::I("P1")}, {term::Var("py"), term::I("P2")}}
	    };
	    assert(r != notExpected_noRow);
	}
    } // namespace test
} // namespace eval


int main () {
    term::test::All();
    eval::test::All();

    return 0;
}


