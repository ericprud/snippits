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
	typedef enum { I_type, L_type, B_type } Type;
	Type t;
	std::string lexicalForm;

	Term (Type t, std::string lexicalForm)
	    : t(t), lexicalForm(lexicalForm) { check(); }

	void check () {
	    assert(t == I_type || t == L_type || t == B_type);
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
	    case Term::I_type: return os << "I("QUOTE << lexicalForm << QUOTE")";
	    case Term::L_type: return os << "L("QUOTE << lexicalForm << QUOTE")";
	    case Term::B_type: return os << "B("QUOTE << lexicalForm << QUOTE")";
	    }
	    assert(false);
	}
    };
    std::ostream& operator<< (std::ostream& os, const Term& ref) {
	return ref.print(os);
    }

    struct I : Term {
	I (std::string lexicalForm)
	    : Term (Term::I_type, lexicalForm) {  }
    };

    struct L : Term {
	L (std::string lexicalForm)
	    : Term (Term::L_type, lexicalForm) {  }
    };

    struct B : Term {
	B (std::string lexicalForm)
	    : Term (Term::B_type, lexicalForm) {  }
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
	typedef enum { Term_type, Var_type } Type;
	Type t;
	Term term;
	Var var;
	TermOrVar (Term term)
	    : t(Term_type), term(term), var(Var("!")) { check(); }
	TermOrVar (Var var)
	    : t(Var_type), term(I("!")), var(var) { check(); }
	void check () {
	    assert(t == Term_type || t == Var_type);
	}
	bool operator== (const TermOrVar& r) const {
	    switch (t) {
	    case Term_type: return r.t == Term_type && term == r.term;
	    case Var_type:  return r.t == Var_type  && var  == r.var;
	    default: assert(false);
	    }
	}
	bool operator< (const TermOrVar& r) const {
	    switch (t) {
	    case Term_type: {
		return
		    r.t == Term_type
		    ? term < r.term
		    : r.t < Term_type;
	    }
	    case Var_type: {
		return
		    r.t == Var_type
		    ? var  < r.var
		    : r.t < Var_type;
	    }
	    default: assert(false);
	    }
	}
	std::ostream& print (std::ostream& os) const {
	    switch (t) {
	    case Term_type:  return os << term;
	    case Var_type:   return os << var;
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
    struct Solution : std::map<term::Var, term::Term> {
	Solution (std::initializer_list<std::map<term::Var, term::Term>::value_type> l) : std::map<term::Var, term::Term>(l) {  }
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
	Multiset (std::initializer_list<Solution> i) : std::list<Solution>(i) {  }
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
	typedef enum { link_type, inv_type, NPS_type,
		       seq_type, alt_type, ZeroOrMorePath_type,
		       OneOrMorePath_type, ZeroOrOnePath_type } Type;
	Type t;
	PathElt (Type t)
	    : t(t) { check(); }
	void check () {
	    assert(t == link_type || t == inv_type || t == NPS_type ||
		   t ==seq_type || t ==alt_type || t ==ZeroOrMorePath_type
		   || t ==OneOrMorePath_type || t ==ZeroOrOnePath_type);
	}
    };

    struct link : PathElt {
	const term::I& i;
	link (const term::I& i)
	    : PathElt(PathElt::link_type), i(i) {  }
    };

    struct inv : PathElt {
	const PathElt& p;
	inv (const PathElt& p)
	    : PathElt(PathElt::inv_type), p(p) {  }
    };

    struct NPS : PathElt {
    	std::set<std::reference_wrapper<const term::I> > s;
    	NPS (std::set<std::reference_wrapper<const term::I> > s)
	    : PathElt(PathElt::NPS_type), s(s) {  }
    };

    struct seq : PathElt {
	const PathElt& l;
	const PathElt& r;
	seq (const PathElt& l, const PathElt& r)
	    : PathElt(PathElt::seq_type), l(l), r(r) {  }
    };

    struct alt : PathElt {
	const PathElt& l;
	const PathElt& r;
	alt (const PathElt& l, const PathElt& r)
	    : PathElt(PathElt::alt_type), l(l), r(r) {  }
    };

    struct ZeroOrMorePath : PathElt {
	const PathElt& p;
	ZeroOrMorePath (const PathElt& p)
	    : PathElt(PathElt::ZeroOrMorePath_type), p(p) {  }
    };

    struct OneOrMorePath : PathElt {
	const PathElt& p;
	OneOrMorePath (const PathElt& p)
	    : PathElt(PathElt::OneOrMorePath_type), p(p) {  }
    };

    struct ZeroOrOnePath : PathElt {
	const PathElt& p;
	ZeroOrOnePath (const PathElt& p)
	    : PathElt(PathElt::ZeroOrOnePath_type), p(p) {  }
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
	    typedef enum { Var_type, BNode_type,  } Type;
	    Type t;
	    term::Var var;
	    term::B bnode;

	    VarOrBNode (term::Var var)
		: t(Var_type), var(var), bnode(term::B("!")) { check(); }
	    VarOrBNode (term::B bnode)
		: t(BNode_type), var(term::Var("!")), bnode(bnode) { check(); }
	    void check () {
		assert(t == Var_type || t == BNode_type);
	    }
	    bool operator== (const VarOrBNode& r) const {
		switch (t) {
		case Var_type:   return r.t == Var_type   && var   == r.var;
		case BNode_type: return r.t == BNode_type && bnode == r.bnode;
		default: assert(false);
		}
	    }
	    bool operator< (const VarOrBNode& r) const {
		switch (t) {
		case Var_type: {
		    return
			r.t == Var_type
			? var < r.var
			: r.t < Var_type;
		}
		case BNode_type: {
		    return
			r.t == BNode_type
			? bnode < r.bnode
			: r.t < BNode_type;
		}
		default: assert(false);																}
	    }
	    std::ostream& print (std::ostream& os) const {
		switch (t) {
		case Var_type:   return os << var;
		case BNode_type: return os << bnode;
		}
		assert(false);
	    }
	};

	/* Solution - same as result::Solution except it permits BNodes.
	 */
        struct Solution : std::map<VarOrBNode, term::Term> {
	    Solution (std::initializer_list<std::map<VarOrBNode, term::Term>::value_type> l) : std::map<VarOrBNode, term::Term>(l) {  }
	    bool freeOrEquals (const VarOrBNode v, const term::Term k) {
		iterator it = find(v);
		if (it == end()) {
		    insert(std::make_pair(v, k));
		    return true;
		}
		return it->second == k;
	    }
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
	    bool matches (const term::TermOrVar fromTP, const term::Term& fromT) {
		switch (fromTP.t) {
		case term::TermOrVar::Term_type: {
		    switch (fromTP.term.t) {
		    case term::Term::I_type: return fromTP.term == fromT;
		    case term::Term::L_type: return fromTP.term == fromT;
		    case term::Term::B_type:
			term::B b(fromTP.term.lexicalForm); // @@ better idea?
			return freeOrEquals(VarOrBNode(b), fromT);
		    }
		}
		case term::TermOrVar::Var_type:
		    return freeOrEquals(VarOrBNode(fromTP.var), fromT);
		}
		assert(false);
	    }
	};

	struct Multiset : std::list<Solution> {
	    Multiset (std::initializer_list<Solution> i) : std::list<Solution>(i) {  }
	    std::ostream& print (std::ostream& os) const {
		for (const_iterator it = begin(); it != end(); ++it) {
		    if (it != begin())
			os << "\n";
		    it->print(os);
		}
		return os;
	    }
	};

	Multiset ret{Solution{}};

	for (graph::BGP::const_iterator tp = BGP.begin(); tp != BGP.end(); ++tp) {
	    for (Multiset::iterator sit = ret.begin(); sit != ret.end(); ) {
		Solution s = *sit;
		sit = ret.erase(sit);
		for (graph::Graph::const_iterator t = G.begin(); t != G.end(); ++t) {
		    Solution s2 = s;
		    if (s2.matches(tp->s, t->s) &&
			s2.matches(tp->p, t->p) &&
			s2.matches(tp->o, t->o))
			ret.insert(sit, s2);
		}
	    }
	}

	result::Multiset r{};
	for (Multiset::const_iterator row = ret.begin(); row != ret.end(); ++row) {
	    result::Solution s {};
	    for (Solution::const_iterator col = row->begin(); col != row->end(); ++col) {
		if (col->first.t != VarOrBNode::BNode_type)
		    s.insert(std::make_pair(col->first.var, col->second));
	    }
	    r.insert(r.end(), s);
	} 

	return r;
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


