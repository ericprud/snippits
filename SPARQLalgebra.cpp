/*
  SPARQLalgebra - test of SPARQL definition of PropertyPath evaluation.

  http://www.w3.org/TR/sparql11-query/#PropertyPathPatterns

  g++ -g -o SPARQLalgebra SPARQLalgebra.cpp -Wall -Werror -std=c++0x && valgrind ./SPARQLalgebra
 */

#include <iostream>
#include <list>
#include <map>
#include <set>
#include <sstream>
#include <cassert>
#include <functional>
// #include <memory>
// std::shared_ptr<int> i(new int(3));

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
	    case Term::Type::I: return os << "I(" << lexicalForm << ")";
	    case Term::Type::L: return os << "L(" << lexicalForm << ")";
	    case Term::Type::B: return os << "B(" << lexicalForm << ")";
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
    bool operator<(const I& a, const I& b) {return a.lexicalForm < b.lexicalForm;}

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
	    os << "Var(" << lexicalForm << ")";
	    return os;
	}
    };
    std::ostream& operator<< (std::ostream& os, const Var& ref) {
	return ref.print(os);
    }

    // Tedious union classes:

    // (RDF-T ∪ V)
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
	    case Type::Term:
		return r.t == Type::Term ? term < r.term : r.t < Type::Term;
	    case Type::Var:
		return r.t == Type::Var ? var  < r.var : r.t < Type::Var;
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

    // (I ∪ V)
    struct IOrVar {
	enum class Type { I, Var };
	Type t;
	I i;
	Var var;
	IOrVar (I i)
	    : t(Type::I), i(i), var(Var("!")) { check(); }
	IOrVar (Var var)
	    : t(Type::Var), i(I("!")), var(var) { check(); }
	void check () {
	    assert(t == Type::I || t == Type::Var);
	}
	bool operator== (const IOrVar& r) const {
	    switch (t) {
	    case Type::I: return r.t == Type::I && i == r.i;
	    case Type::Var:  return r.t == Type::Var  && var  == r.var;
	    default: assert(false);
	    }
	}
	bool operator< (const IOrVar& r) const {
	    switch (t) {
	    case Type::I:
		return r.t == Type::I ? i < r.i : r.t < Type::I;
	    case Type::Var:
		return r.t == Type::Var ? var  < r.var : r.t < Type::Var;
	    default: assert(false);
	    }
	}
	std::ostream& print (std::ostream& os) const {
	    switch (t) {
	    case Type::I:  return os << i;
	    case Type::Var:   return os << var;
	    }
	    assert(false);
	}
    };

    std::ostream& operator<< (std::ostream& os, const IOrVar& iov) {
	return iov.print(os);
    }


    namespace test {
	void All () {
	    I i1("iri-"), i2("iri-"), i3("iri3");
	    // std::cout << i1 << (i1 == i2 ? "==" : "!=") << i2 << std::endl;
	    // std::cout << i1 << (i1 == i3 ? "==" : "!=") << i3 << std::endl;
	    assert(i1 == i2);
	    assert(!(i1 == i3));

	    I i("X"); B b("X"); L l("X");
	    assert(!(i == b) && !(b == l) && !(i == l));
	}
    };

} // namespace term;


namespace graph {
    struct Triple {
	term::Term s;
	term::I p;
	term::Term o;
	Triple (term::Term s, term::I p, term::Term o)
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
	term::TermOrVar s;
	term::IOrVar p;
	term::TermOrVar o;
	TriplePattern (term::TermOrVar s, term::IOrVar p, term::TermOrVar o)
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

    // Definition: Compatible Mappings
    // http://www.w3.org/2009/sparql/docs/query-1.1/rq25.xml#defn_algCompatibleMapping
    bool CompatibleMappings (const Solution& mu1, const Solution& mu2) {
	for (Solution::const_iterator l = mu1.begin(); l != mu1.end(); ++l) {
	    Solution::const_iterator r = mu2.find(l->first);
	    if (r != mu2.end() && !(l->second == r->second))
		return false;
	}
	return true;
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

    Multiset Join (Multiset& l, Multiset& r) {
	Multiset ret {};
	for (Multiset::const_iterator lit = l.begin(); lit != l.end(); ++lit)
	    for (Multiset::const_iterator rit = r.begin(); rit != r.end(); ++rit)
		if (CompatibleMappings(*lit, *rit)) {
		    Solution s {};
		    for (auto lz: *lit) s.insert(lz);
		    for (auto rz: *rit) s.insert(rz);
		    ret.insert(ret.end(), s);
		}
		    
	return ret;
    }

    // Definition: Union
    // http://www.w3.org/2009/sparql/docs/query-1.1/rq25.xml#defn_algUnion
    Multiset Union (Multiset& l, Multiset& r) {
	Multiset ret {};
	for (auto row: l) ret.insert(ret.end(), row);
	for (auto row: r) ret.insert(ret.end(), row);
	return ret;
    }

    // Definition: Project
    // http://www.w3.org/2009/sparql/docs/query-1.1/rq25.xml#defn_algProjection
    Multiset Project (Multiset& m, std::set<term::Var> PV) {
	Multiset ret {};
	for (Multiset::const_iterator mit = m.begin(); mit != m.end(); ++mit) {
	    Solution mu {};
	    for (auto sit: *mit)
		if (PV.find(sit.first) != PV.end())
		    mu.insert(sit);
	    ret.insert(ret.end(), mu);	    
	}
	return ret;
    }

    namespace test {
	void All () {
	    {
		result::Solution s1{{term::Var("l"),  term::L("L1")}, {term::Var("i"), term::I("I1")}};
		result::Solution s2{{term::Var("l2"), term::L("L1")}, {term::Var("i"), term::I("I1")}};
		result::Solution s3{{term::Var("l2"), term::L("L1")}, {term::Var("i"), term::I("I2")}};
		assert(CompatibleMappings(s1, s2));
		assert(!CompatibleMappings(s2, s3));
	    }

	    {
		result::Multiset l {
		    //  l  i  b
		    // L1 I1 --
		    // -- I2 B2
		    result::Solution {{term::Var("l"), term::L("L1")}, {term::Var("i"), term::I("I1")}},
		    result::Solution {{term::Var("b"), term::B("B2")}, {term::Var("i"), term::I("I2")}}
		};
		result::Multiset r {
		    // l2  i  b
		    // L0 I0 -- xx
		    // L1 I1 --
		    // L2 I1 --
		    // -- I2 B2
		    result::Solution {{term::Var("l2"), term::L("L0")}, {term::Var("i"), term::I("I0")}},
		    result::Solution {{term::Var("l2"), term::L("L1")}, {term::Var("i"), term::I("I1")}},
		    result::Solution {{term::Var("l2"), term::L("L2")}, {term::Var("i"), term::I("I1")}},
		    result::Solution {{term::Var("b") , term::B("B2")}, {term::Var("i"), term::I("I2")}}
		};

		{
		    result::Multiset join {
			//  l  i  b l2
			// L1 I1 -- L1
			// L1 I1 -- L2
			// -- I2 B2 --
			result::Solution {{term::Var("l"), term::L("L1")}, {term::Var("i"), term::I("I1")}, {term::Var("l2"), term::L("L1")}},
			result::Solution {{term::Var("l"), term::L("L1")}, {term::Var("i"), term::I("I1")}, {term::Var("l2"), term::L("L2")}},
			result::Solution {{term::Var("b"), term::B("B2")}, {term::Var("i"), term::I("I2")}}
		    };
		    assert(Join(l, r) == join);
		}

		{
		    result::Multiset onion {
			result::Solution {{term::Var("l"), term::L("L1")}, {term::Var("i"), term::I("I1")}},
			result::Solution {{term::Var("b"), term::B("B2")}, {term::Var("i"), term::I("I2")}},
			result::Solution {{term::Var("l2"), term::L("L0")}, {term::Var("i"), term::I("I0")}},
			result::Solution {{term::Var("l2"), term::L("L1")}, {term::Var("i"), term::I("I1")}},
			result::Solution {{term::Var("l2"), term::L("L2")}, {term::Var("i"), term::I("I1")}},
			result::Solution {{term::Var("b") , term::B("B2")}, {term::Var("i"), term::I("I2")}}
		    };
		    assert(Union(l, r) == onion);
		}
	    }
	}
    } // namespace test
} // namespace result


namespace eval {

    // Definition: Basic Graph Pattern Matching
    // http://www.w3.org/TR/sparql11-query/#BGPsparql
    result::Multiset BasicGraphPatternMatching (graph::BGP& BGP, graph::Graph& G) {

	// term::VarOrBNode captures the types serving as variables in a Solution.
	// Including BNodes allows one algorithm to handle both variable binding
	// and BNode-isomorphism.
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

	// Definition: Pattern Instance Mapping
	// http://www.w3.org/2009/sparql/docs/query-1.1/rq25.xml#defn_PatternInstanceMapping
	// PatternInstanceMapping - same as result::Solution except it permits BNodes.
	typedef std::map<VarOrBNode, term::Term> PatternInstanceMappingContainer;
        struct PatternInstanceMapping : PatternInstanceMappingContainer {
	    PatternInstanceMapping (std::initializer_list<PatternInstanceMappingContainer::value_type> l)
		: PatternInstanceMappingContainer(l) {  }
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
	    bool matches (const term::IOrVar fromTP, const term::I& fromT) {
		switch (fromTP.t) {
		case term::IOrVar::Type::I:
		    return fromTP.i == fromT;
		case term::IOrVar::Type::Var:
		    return freeOrEquals(VarOrBNode(fromTP.var), fromT);
		}
		assert(false);
	    }
	};

	// Multiset - same as result::Multiset except it permits BNodes.
	struct Multiset : std::list<PatternInstanceMapping> {
	    Multiset (std::initializer_list<PatternInstanceMapping> i)
		: std::list<PatternInstanceMapping>(i) {  }
	    std::ostream& print (std::ostream& os) const {
		for (const_iterator it = begin(); it != end(); ++it) {
		    if (it != begin())
			os << "\n";
		    it->print(os);
		}
		return os;
	    }
	};

	Multiset bindings{PatternInstanceMapping{}}; // 1 row, 0 var/bnode bindings.

	// Explore permutations of the graph, populating var/bnode bindings.
	for (graph::BGP::const_iterator tp = BGP.begin(); tp != BGP.end(); ++tp) {
	    for (Multiset::iterator sit = bindings.begin(); sit != bindings.end(); ) {
		PatternInstanceMapping s = *sit;
		sit = bindings.erase(sit);
		for (graph::Graph::const_iterator t = G.begin(); t != G.end(); ++t) {
		    PatternInstanceMapping s2 = s;
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
	    for (PatternInstanceMapping::const_iterator col = row->begin(); col != row->end(); ++col) {
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


namespace path {
    struct link;
    struct inv;
    struct NPS;
    struct seq;
    struct alt;
    struct ZeroOrMorePath;
    struct OneOrMorePath;
    struct ZeroOrOnePath;

    struct PathElt {
	enum class Type {
	    link, inv, NPS, seq, alt, ZeroOrMorePath, OneOrMorePath, ZeroOrOnePath
	};
	Type t;
	PathElt (Type t)
	    : t(t) { check(); }
	PathElt (const PathElt& ref)
	    : t(ref.t) { assert(false); }

	void check () {
	    assert(t == Type::link || t == Type::inv || t == Type::NPS ||
		   t == Type::seq || t == Type::alt || t == Type::ZeroOrMorePath
		   || t == Type::OneOrMorePath || t == Type::ZeroOrOnePath);
	}
	virtual const link& get_link () const { assert(false); }
	virtual const inv& get_inv () const { assert(false); }
	virtual const NPS& get_NPS () const { assert(false); }
	virtual const seq& get_seq () const { assert(false); }
	virtual const alt& get_alt () const { assert(false); }
	virtual const ZeroOrMorePath& get_ZeroOrMorePath () const { assert(false); }
	virtual const OneOrMorePath& get_OneOrMorePath () const { assert(false); }
	virtual const ZeroOrOnePath& get_ZeroOrOnePath () const { assert(false); }
    };

    struct link : PathElt {
	const term::I i;
	link (const term::I i)
	    : PathElt(PathElt::Type::link), i(i) { get_link(); }
	virtual const link& get_link () const { return *this; }
    };

    struct inv : PathElt {
	const PathElt& p;
	inv (const PathElt& p)
	    : PathElt(PathElt::Type::inv), p(p) {  }
	virtual const inv& get_inv () const { return *this; }
    };

    struct NPS : PathElt {
    	std::set<std::reference_wrapper<const term::I> > s;
    	NPS (std::set<std::reference_wrapper<const term::I> > s)
	    : PathElt(PathElt::Type::NPS), s(s) {  }
	virtual const NPS& get_NPS () const { return *this; }
    };

    struct seq : PathElt {
	const PathElt& l;
	const PathElt& r;
	seq (const PathElt& l, const PathElt& r)
	    : PathElt(PathElt::Type::seq), l(l), r(r) {  }
	virtual const seq& get_seq () const { return *this; }
    };

    struct alt : PathElt {
	const PathElt& l;
	const PathElt& r;
	alt (const PathElt& l, const PathElt& r)
	    : PathElt(PathElt::Type::alt), l(l), r(r) {  }
	virtual const alt& get_alt () const { return *this; }
    };

    struct ZeroOrMorePath : PathElt {
	const PathElt& p;
	ZeroOrMorePath (const PathElt& p)
	    : PathElt(PathElt::Type::ZeroOrMorePath), p(p) {  }
	virtual const ZeroOrMorePath& get_ZeroOrMorePath () const { return *this; }
    };

    struct OneOrMorePath : PathElt {
	const PathElt& p;
	OneOrMorePath (const PathElt& p)
	    : PathElt(PathElt::Type::OneOrMorePath), p(p) {  }
	virtual const OneOrMorePath& get_OneOrMorePath () const { return *this; }
    };

    struct ZeroOrOnePath : PathElt {
	const PathElt& p;
	ZeroOrOnePath (const PathElt& p)
	    : PathElt(PathElt::Type::ZeroOrOnePath), p(p) {  }
	virtual const ZeroOrOnePath& get_ZeroOrOnePath () const { return *this; }
    };

    struct Path {
	const term::TermOrVar s;
	const PathElt& p;
	const term::TermOrVar o;
	Path (const term::TermOrVar s, const PathElt& p, const term::TermOrVar o)
	    : s(s), p(p), o(o) {  }
    };

    result::Multiset eval (path::Path path, graph::Graph& G) {
	switch (path.p.t) {

	// Definition: Evaluation of Predicate Property Path
	// http://www.w3.org/2009/sparql/docs/query-1.1/rq25.xml#defn_evalPP_predicate
	case PathElt::Type::link:
	    {
		graph::BGP bgp {
		    graph::TriplePattern(path.s, path.p.get_link().i, path.o)
		};
		return eval::BasicGraphPatternMatching(bgp, G);
	    }

	// Definition: Evaluation of Inverse Property Path
	// http://www.w3.org/2009/sparql/docs/query-1.1/rq25.xml#defn_evalPP_inverse
	case PathElt::Type::inv:
	    return eval(Path(path.o, path.p.get_inv().p, path.s), G);

	// Definition: Evaluation of NegatedPropertySet
	// http://www.w3.org/2009/sparql/docs/query-1.1/rq25.xml#eval_negatedPropertySet
	// @@ what's the function triple(X, p, Y) mean?
	// how about { evaluation of basic graph pattern {X p Y} | p ∉ S } per link(iri)?
	case PathElt::Type::NPS:
	    {
		const path::NPS& as_nps = path.p.get_NPS();

		std::stringstream ss; ss << &path.p;
		term::Var fresh(ss.str()); // varname is address of path.

		graph::BGP bgp {
		    graph::TriplePattern(path.s, fresh, path.o)
		};
		result::Multiset mu = eval::BasicGraphPatternMatching(bgp, G);
		result::Multiset ret {};
		for (result::Solution& row: mu) {
		    const term::I i(row.find(fresh)->second.lexicalForm);
		    if (as_nps.s.find(i) == as_nps.s.end())
			ret.insert(ret.end(), row);
		}
		std::set<term::Var> vars {  };
		// @@ add to definition
		if (path.s.t == term::TermOrVar::Type::Var)
		    vars.insert(path.s.var);
		if (path.o.t == term::TermOrVar::Type::Var)
		    vars.insert(path.o.var);
		return Project(ret, vars);
	    }

	// Definition: Evaluation of Sequence Property Path
	// http://www.w3.org/2009/sparql/docs/query-1.1/rq25.xml#defn_evalPP_sequence
	case PathElt::Type::seq:
	    {
		std::stringstream ss; ss << &path.p;
		term::Var fresh(ss.str()); // varname is address of path.

		result::Multiset l = eval(Path(path.s, path.p.get_seq().l, fresh), G);
		result::Multiset r = eval(Path(fresh, path.p.get_seq().r, path.o), G);
		result::Multiset A = Join(l, r);
		std::set<term::Var> vars {  };
		// @@ add to definition
		if (path.s.t == term::TermOrVar::Type::Var)
		    vars.insert(path.s.var);
		if (path.o.t == term::TermOrVar::Type::Var)
		    vars.insert(path.o.var);
		return Project(A, vars);
	    }

	// Definition: Evaluation of Alternative Property Path
	// http://www.w3.org/2009/sparql/docs/query-1.1/rq25.xml#defn_evalPP_alternative
	case PathElt::Type::alt:
	    {
		result::Multiset l = eval(Path(path.s, path.p.get_alt().l, path.o), G);
		result::Multiset r = eval(Path(path.s, path.p.get_alt().r, path.o), G);
		return Union(l, r);
	    }
	case PathElt::Type::ZeroOrMorePath: assert(false);
	case PathElt::Type::OneOrMorePath: assert(false);
	case PathElt::Type::ZeroOrOnePath: assert(false);
	}
	return result::Multiset::Multiset0;
    }

    namespace test {
	void All () {
	    graph::Graph g {
		graph::Triple(term::I("n1"), term::I("p1"), term::I("n2")),
		graph::Triple(term::I("n2"), term::I("p2"), term::L("l3")),
		graph::Triple(term::I("n2"), term::I("p3"), term::L("l3"))
	    };

	    result::Multiset expected {
		result::Solution {{term::Var("s"), term::I("n1")}, {term::Var("o"), term::I("n2")}}
	    };

	    {
		link l(term::I("p1"));
		Path p(term::Var("s"), l, term::Var("o"));
		assert(eval(p, g) == expected);
	    }

	    {
		link l(term::I("p1"));
		inv i(l);
		Path p(term::Var("o"), i, term::Var("s"));
		assert(eval(p, g) == expected);
	    }

	    {
		link l(term::I("p1"));
		link r(term::I("p2"));
		seq s(l, r);
		Path p(term::Var("s"), s, term::Var("o"));
		result::Multiset exp {
		    result::Solution {{term::Var("s"), term::I("n1")}, {term::Var("o"), term::L("l3")}}
		};
		assert(eval(p, g) == exp);
	    }

	    {
		link l(term::I("p1"));
		link r(term::I("p2"));
		alt a(l, r);
		Path p(term::Var("s"), a, term::Var("o"));
		result::Multiset exp {
		    result::Solution {{term::Var("s"), term::I("n1")}, {term::Var("o"), term::I("n2")}},
		    result::Solution {{term::Var("s"), term::I("n2")}, {term::Var("o"), term::L("l3")}}
		};
		assert(eval(p, g) == exp);
	    }

	    {
		term::I p1("p1");
		term::I p2("p2");
		std::set<std::reference_wrapper<const term::I> >s {p1, p2};
		NPS n(s);
		Path p(term::Var("s"), n, term::Var("o"));
		const path::NPS& as_nps = p.p.get_NPS();
		term::I i("p1");
		if (as_nps.s.find(i) == as_nps.s.end())
		    assert(false);
		result::Multiset exp {
		    result::Solution {{term::Var("s"), term::I("n2")}, {term::Var("o"), term::L("l3")}}
		};
		assert(eval(p, g) == exp);
	    }

	    // result::Multiset expected {
	    // 	result::Solution {{term::Var("px"), term::I("p1")}, {term::Var("py"), term::I("p2")}},
	    // 	result::Solution {{term::Var("px"), term::I("p1")}, {term::Var("py"), term::I("p3")}}
	    // };
	}
    }
} // namespace path;


int main () {
    term::test::All();
    eval::test::All();
    result::test::All();
    path::test::All();

    return 0;
}


