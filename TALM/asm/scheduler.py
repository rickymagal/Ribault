# FlowASM - A Trebuchet assembler for TALM's dataflow graphs.
# Authors - Tiago A.O.A. <tiagoaoa@cos.ufrj.br> Leandro J. Marzulo <lmarzulo@cos.ufrj.br>
#
# THIS IS NOT AN OFFICIAL RELEASE
# DO NOT COPY OR DISTRIBUTE
#



import os
import re
import random
import sys
import heapq
from functools import reduce
import debug

DEFAULT_COST = 100
COMM_COST_FACTOR = float(os.environ.get("COMM_COST_FACTOR", "1.0"))
COMM_COST_FACTOR = max(COMM_COST_FACTOR, 0.0)
BALANCE_ONLY = os.environ.get("BALANCE_ONLY", "0") == "1"
# Load-balance bias. 0 keeps legacy behavior, >0 trades comm cost for balance.
BALANCE_ALPHA = float(os.environ.get("BALANCE_ALPHA", "0.25"))
# Super-graph construction is not used by placement; keep it opt-in for perf.
BUILD_SUPERGRAPH = os.environ.get("BUILD_SUPERGRAPH", "0") == "1"
DEFAULT_SUPER_AVGTIME = 1000
DEFAULT_SIMPLE_AVGTIME = 5

SUPER_OPS = ('super', 'specsuper', 'superi', 'specsuperi')
INCTAG_OPS = ('inctag', 'inctagi')

class flist(list):
        """Specialized list to use l.remove() as part of a statement"""
        def remove(self, item):
                if isinstance(item, list):
                        for x in item: self.remove(x)
                else:
                        super(flist, self).remove(item)
                return self #Returns the list itself


        def __sub__(self, other):
                return flist(self).remove(other)

class InstNode:
        def __init__(self, tks, avgtimes):
                name = tks[1]
                if isinstance(name, list):
                        self.names = list(name)
                        self.name = self.names[-1] if len(self.names) > 0 else "instruction_anon"
                else:
                        self.names = [name]
                        self.name = name
                self.op = tks[0]
                (self.sources, self.input_num) = self.getsources(tks)
                self.dests = []
                self.n_inedges = 0
                self.min_start = 0
        

                if self.op in ('super', 'specsuper', 'superi', 'specsuperi') and int(tks[2]) in avgtimes: #tks[2] is the blocknumber
                        blocknumber = int(tks[2]) #blocknumber of the superinstruction
                        debug.print_debug("INFO", "AVGTIME for %s blocknum %d: %d" %(self.op, blocknumber, avgtimes[blocknumber]))
                        self.avgtime = avgtimes[blocknumber]
                else:

                        if self.op in ('super', 'specsuper', 'superi', 'specsuperi'):
                                debug.print_debug("INFO", "Forcing super AVGTIME %s" %self.op)
                                self.avgtime = DEFAULT_SUPER_AVGTIME
                        else:
                                debug.print_debug("INFO", "Regular AVGTIME %s" %self.op)
                                self.avgtime = DEFAULT_SIMPLE_AVGTIME

                                if self.op == 'const': #for testing
                                        self.avgtime = 1        


        def getsources(self, tks):
                sources = []
                input_num = 0

                if self.is_super():
                        start = 4
                else:
                        start = 2
                for operand in tks[start:]:
                        if isinstance(operand, list) or re.match("^[aA-zZ]", operand):
                                input_num += 1
                                sources += [operand]

                return (sources,input_num)



        def comp_prob(self, sched): #compute the probabilitty of instr's execution
                instr = self
                outprob = 1

                eprobs = sched.edgeprobs
                #for inport in range(instr.inport_num):
                inport = 0
                """we are using the probability of only one of the input ports, see the comment on the SteerNode class."""
                if instr.input_num > 0:
                        inedges = sched._pred_map.get((instr, inport), [])
                        #outprob *= reduce(lambda acc,p: acc+p, [sched.edgeprobs[e] for e in inedges])
                
                        outprob = reduce(lambda acc,p: acc+p, [sched.edgeprobs.get(e, 1.0) for e in inedges])
        


                for (sp, di, dp) in sched._succ_map.get(instr, []):
                        sched.edgeprobs[(instr, sp, di, dp)] = outprob

                return outprob


        def is_super(self):
                return self.op in SUPER_OPS

        def is_inctag(self):
                return self.op in INCTAG_OPS
class SteerNode(InstNode):

        def comp_probs_cond(self, sched, boolval):
                port = boolval and "t" or "f"
                debug.print_debug("INFO", "Steer Node!!! Para porta %d"  %boolval)

                instr = self
                eprobs = sched.edgeprobs

                inedges = [sched._pred_map.get((instr, 0), [])]
                
                
#               inedges += [[(si, sp, di, dp) for (si, sp, di, dp) in sched.edges if di == instr and dp == 1]]

#               outprob = reduce(lambda acc,p: acc+p, [sched.edgeprobs[e] for e in inedges[1]])
                
                """commented out, since we are only using the probabily of one of the input ports,
                assuming that the dataflow graph is well-formed."""

                debug.print_debug("INFO", [(si.name, sp, di.name) for (si, sp, di, dp) in inedges[0]])
                conds = [eprobs.get((si, sp, di, dp), 1.0)*sched.getvarprob(si.name, boolval) for (si, sp, di, dp) in inedges[0]] 
                debug.print_debug("INFO", conds)
                
                outprob = reduce(lambda acc,p: acc+p, conds)
                
                outedges = [(instr, sp, di, dp) for (sp, di, dp) in sched._succ_map.get(instr, []) if sp == port]
                #debug.print_debug("INFO", "Steer outprob: %s %s" %(si.name, sched.getvarprob(si.name, boolval)))
                for edge in outedges:
                        sched.edgeprobs[edge] = outprob


                return outprob



        def comp_prob(self, sched):
                return (self.comp_probs_cond(sched, True) + self.comp_probs_cond(sched, False))
        
        



class Processor:
        def __init__(self, index, num_procs):
                self.comm_cost =  [p != index and DEFAULT_COST * COMM_COST_FACTOR or 0 for p in range(num_procs)]
                self.mkspan = 0
                self.index = index
                self.load = 0.0

class GraphBuilder:
        

        def __init__(self, num_procs):
                
                self.instructions = {} 
                self.ordered_instructions = []

                self.dests = {}
                self.edges = []
                self.edgeprobs = {}
                self.instcount = 0
                
                #num_procs = 5 #TODO: must come from a parameter

                self.processors = [Processor(index, num_procs) for index in range(num_procs)]
                self.mkspan = [0 for i in range(num_procs)]
                self._path_cache = {}


        def start(self):
                pass
        def exit(self):
                #debug.print_debug("INFO", "GraphBuilder: %s" %self.edges)
                #debug.print_debug("INFO", self.instructions)
                debug.print_debug("INFO", "Autoplacement: Building graph")
                self.build_graph(self.instructions, self.edges)

                debug.print_debug("INFO", "Autoplacement: Initiating autoplacement")
                self.traverse_graph(self.instructions)
                debug.print_debug("INFO", "Finish traversing")
                debug.print_debug("INFO", "Placement:")
                for (name, instr) in [(i.name, i) for i in self.ordered_instructions]:
                        if not hasattr(instr, "proc"):
                                instr.proc = self.processors[0]
                                instr.min_start = 0
                        priority = getattr(instr, "priority", 0)
                        debug.print_debug("INFO", "%s: %d (p = %d)" %(name, instr.proc.index, priority) )

                placement = [instr.proc.index if hasattr(instr, "proc") else 0 for instr in self.ordered_instructions]
                
                debug.print_debug("INFO", "Manual Placement: %s" %placement)
                
                debug.print_debug("INFO", "mkspans:")
                for proc in self.processors:
                        debug.print_debug("INFO", "%s: %f" %(proc.index, proc.mkspan))
                debug.print_debug("INFO", "Edge probs: %s" %[(s[0][0].name, s[0][2].name, s[1]) for s in self.edgeprobs.items()])
                self.writefile(self.outfile, placement)         


        def writefile(self, file, placement):
                file.write("%d\n" %len(placement))
                for inst in placement:
                        file.write("%d\n" %inst)
                file.close()



        def getvarprob(self, var, boolval):
                profile = self.profile
                debug.print_debug("INFO", "getvarprob")
                if var in profile.varprobs:
                        debug.print_debug("INFO", "P(%s = 1) = %f" %(var, profile.varprobs[var]))
                        return boolval and profile.varprobs[var] or (1 - profile.varprobs[var])

                else:
                        debug.print_debug("INFO", "No probability for control variable '%s'; using 0.5" %var)
                        return 0.5


        def choose_proc(self, instr):
                min_start = getattr(sys, "maxint", sys.maxsize)
                
                if len(self.processors) == 1:
                        return (self.processors[0], 0)

                bestproc = -1
                bestscore = float("inf")
                plist = self.processors

                if BALANCE_ONLY:
                        # Pure load balance, ignore comm costs and min_start.
                        bestproc = min(plist, key=lambda p: p.load)
                        return (bestproc, bestproc.mkspan)

                inedges = []
                for dp in range(instr.input_num):
                        inedges.extend(self._pred_map.get((instr, dp), []))
                srclist = [(si, si.proc, self.edgeprobs.get((si, sp, di, dp), 1.0)) for (si, sp, di, dp) in inedges]
                for proc in plist:
                        debug.print_debug("INFO", "in proc %d: mkspan %f - edges %s" %(proc.index, proc.mkspan, [(si.min_end + sp.comm_cost[proc.index])*prob for (si, sp, prob) in srclist]))
                        #tmpstart = max([0] + [(si.min_end + sp.comm_cost[proc.index])*prob for (si, sp, prob) in srclist])
                        if len(srclist) > 0: 
                                (maxsi, maxsp, maxprob) = max(srclist, key=lambda item: (item[0].min_end + item[1].comm_cost[proc.index]) * item[2])
                                        #si = source instruction ; sp = source processor, 
                                        #which is the processor in which the source instruction was placed      
                        
                                tmpstart = maxsi.min_end + maxsp.comm_cost[proc.index]
                                debug.print_debug("INFO", "tmpstart: %f %f %f" %(tmpstart, maxsi.min_end, maxsp.comm_cost[proc.index]))
                        else:
                                tmpstart = 0
                        tmpstart = max([proc.mkspan, tmpstart])

                        score = tmpstart + (BALANCE_ALPHA * proc.load)
                        if score < bestscore - 1e-12:
                                bestscore = score
                                min_start = tmpstart
                                bestproc = proc
                        elif abs(score - bestscore) <= 1e-12:
                                if bestproc == -1 or proc.load < bestproc.load:
                                        bestproc = proc


                debug.print_debug("INFO", "Best is %d - min_start = %f (score=%f)" % (bestproc.index, min_start, bestscore))
                return (bestproc, min_start)

        


        def traverse_graph(self, instructions):
                heap = []
                heap_counter = 0
                succ_map = self._succ_map
                heappush = heapq.heappush
                heappop = heapq.heappop
                debug.print_debug("INFO", "Testing %s" %instructions)
                for (name, instr) in instructions.items():
                        if instr.n_inedges == 0:
                                heappush(heap, (-instr.priority, heap_counter, instr))
                                heap_counter += 1

                while heap:
                        _negp, _i, instr = heappop(heap)
                        self.visit(instr)
                
                        for (_sp, dstinstr, _dp) in succ_map.get(instr, []):
                                dstinstr.n_inedges -= 1

                                if dstinstr.n_inedges == 0:
                                        debug.print_debug("INFO", "Adding %s to ready list" %dstinstr.name)
                                        heappush(heap, (-dstinstr.priority, heap_counter, dstinstr))
                                        heap_counter += 1
                        debug.print_debug("INFO", "Ready: %d" %len(heap))


        def visit(self, instr):
                #place the nodec
                debug.print_debug("INFO", "Visiting %s" %instr.name)

                (instr.proc, instr.min_start) = self.choose_proc(instr)
                processor = instr.proc
                debug.print_debug("INFO", "Chosen proc is %d" %(instr.proc.index))
                #instr.min_start = processor.mkspan

                prob = instr.comp_prob(self)
                instr.min_end = processor.mkspan = instr.min_start + instr.avgtime
                processor.load += instr.avgtime

                debug.print_debug("INFO", "Min_start = %f Min_end = %f prob = %f" %(instr.min_start, instr.min_end, instr.comp_prob(self)))
                """the instr.comp_prob function also propagates the probability to the outgoing edges"""
                

        def build_graph(self, instrs, edges):
                edges_ref = []
                for edge in self.edges:
                        srcinstr = instrs[edge[0][0]]
                        srcport = edge[0][1]
                        dstinstr = instrs[edge[1][0]]
                        dstport = edge[1][1]
                                                         
                        edges_ref += [(srcinstr, srcport, dstinstr, dstport)]
                        dstinstr.n_inedges += 1

                #debug.print_debug("INFO", "%s %s" %(srcinstr.name, [x[1].name for x in srcinstr.dests]))
                self.edges = edges_ref #store the edges with reference to the InstNode objetcts, instead of the instruction names
                self._succ_map = self._build_succ_map(self.edges)
                self._pred_map = self._build_pred_map(self.edges)
                self.return_edges = []
                self.return_edge_pairs = None
                roots = [i for i in self.ordered_instructions if i.input_num == 0]

                self.tag_return_edges(self.edges, list(roots))
                if BUILD_SUPERGRAPH:
                        self.build_super_graph(roots) 
                self.remove_loops(self.return_edges)
                self._succ_map = self._build_succ_map(self.edges)
                self._pred_map = self._build_pred_map(self.edges)
                
                for instr in roots:
                        self.set_priority(instr)


        def set_priority(self, instr):
                dests = [d for (_sp, d, _dp) in self._succ_map.get(instr, [])]
                if not hasattr(instr, "priority"):
                        # Break cycles in recursive graphs by treating back-edges as leafs.
                        if getattr(instr, "_prio_visiting", False):
                                return instr.avgtime
                        instr._prio_visiting = True
                        if len(dests) == 0:
                                instr.priority = instr.avgtime
                        else:
                                instr.priority = max([self.set_priority(d) for d in dests]) + instr.avgtime
                        instr._prio_visiting = False
                return instr.priority

        def tag_return_edges(self, edges, ready):
                #TODO: use vertex depths (distance from each root) to establish dependency between the inctag and the instruction from which the return edge comes.

                # Cache path queries for this pass; graph is static here.
                self._path_cache = {}
                counters = dict([(inst, inst.input_num) for inst in self.ordered_instructions])
                marked_ports = set()
                marked_instrs = set()
                ready = list(ready)
                pop_ready = ready.pop

                while ready:
                        instr = pop_ready()
                        outedges = [(instr, s_p, d, d_p) for (s_p, d, d_p) in self._succ_map.get(instr, [])]
                        for (srcinst, srcport, dstinst, dstport) in outedges:
                                key = (dstinst, dstport)
                                if key not in marked_ports:
                                        marked_ports.add(key)
                                        counters[dstinst] -= 1
                                        count = counters[dstinst]
                                        if count == 0:
                                                ready.append(dstinst)
                                                marked_instrs.add(dstinst)
                                                debug.print_debug("INFO", "Marking %s" %dstinst.name)
                                else:
                                
                                        if dstinst in marked_instrs and dstinst.op == "inctag":
                                                debug.print_debug("INFO", "Testing Path <%s, %s>" %(dstinst.name, srcinst.name))
                                                if  self.haspath(dstinst, srcinst, self.edges):
                                                        debug.print_debug("INFO", "Loop detected %s %s" %(srcinst.name, dstinst.name))
                                                        e = (srcinst, srcport, dstinst, dstport)
                                                        #self.edges.remove(e)
                                                        self.return_edges.append(e)
                                                        dstinst.n_inedges -= 1
                                

        
        def remove_loops(self,return_edges):
                for e in return_edges:
                        self.edges.remove(e)


                        
        def create_edges(self, dstname, dstport, source):
                edges = []

                def _flatten_sources(src):
                        if isinstance(src, list):
                                out = []
                                for s in src:
                                        out += _flatten_sources(s)
                                return out
                        return [src]

                for source in _flatten_sources(source):
        
                        srcname = source.split('.')[0]
                        
                        if re.match(r".*\.[tf]$", source):
                                
                                srcport = source.split('.')[1]
                        else:
                                srcport = None #we only need to distinguish the ports of a steer
                        edges += [((srcname, srcport), (dstname, dstport))]

                return edges    
                        
                
                

        def asmline(self, tks):
                op = tks[0]
                if op == "steer": #TODO: maybe find a way to use the class hierarchy present in the flowasm.py assembler
                        debug.print_debug("INFO", "Instruction is Steer")
                        instr = SteerNode(tks, self.profile.avgtimes)
                else:
                        instr = InstNode(tks, self.profile.avgtimes)
                
                for name in instr.names:
                        self.instructions[name] = instr
                self.ordered_instructions += [instr]

                if instr.sources != None:
                        for destport, source in zip(range(len(instr.sources)), instr.sources):
                                self.edges += self.create_edges(instr.name, destport, source)



        def haspath(self, u, v, edges):
                """Check if there is a path between u and v."""
                key = (u, v)
                cached = self._path_cache.get(key)
                if cached is not None:
                        return cached
                # Use adjacency and iterative DFS to avoid quadratic recursion.
                stack = [u]
                visited = set()
                succ_map = self._succ_map
                while stack:
                        cur = stack.pop()
                        if cur == v:
                                debug.print_debug("INFO", "Found path: (%s,%s)" %(u, v))
                                self._path_cache[key] = True
                                return True
                        if cur in visited:
                                continue
                        visited.add(cur)
                        stack.extend([dst for (_sp, dst, _dp) in succ_map.get(cur, [])])
                self._path_cache[key] = False
                return False




        def build_super_graph(self, roots):
                """Builds a graph that contains only the super-instructions. In this graph, and edge (u, v) exists if in the original graph there is a path between u and v such that there is no super-instruction in the path other than u and v."""
                self.superedges = set()
                self.supers = []
                self.stack_super_graph = []
                self.stack_super_graph_set = set()
                self.super_ret_edges = set()
                self.pending_superedges = set()
                self.pending_ret_edges = set()
                self._supers_memo = {}

                for instr in roots:
                        instr.inside_loop = False
                        if instr.is_super():
                                prev = instr
                        else:
                                prev = None
                        for dest in [di for (_sp, di, _dp) in self._succ_map.get(instr, [])]:
                                self.traverse_supers(prev, dest)

                for (src, dest) in self.pending_superedges:
                        for destsuper in dest.nearest_supers:
                                debug.print_debug("INFO", "later adding edge %s %s" %(src.name, destsuper.name))
                                self.superedges |= set([(src, destsuper)])
                        
                                if (src, dest) in self.pending_ret_edges and src.inside_loop:
                                        debug.print_debug("INFO", "Adding pending %s %s" %(src.name, destsuper.name))
                                        self.super_ret_edges |= set([(src, destsuper)])

                debug.print_debug("INFO", [instr.name for instr in self.supers])
                debug.start_debug("3")
                debug.print_debug("INFO", [(a.name, b.name) for (a,b) in  self.superedges])
                debug.print_debug("INFO", [(a.name, b.name) for (a,b) in  self.super_ret_edges])


        def traverse_supers(self, prev, instr, ret_edge_in_path=False, inctag_in_path=False):
                instr.nearest_supers = set()
                #nearest_supers stores all supers s such that there is a path <instr, s> where there are no other supers other than instr and s

                # Fast path: only cache the default case (no return edge, no inctag)
                # and only when the node is not on the current recursion stack.
                if (not ret_edge_in_path) and (not inctag_in_path) and (not instr.is_super()) and instr not in self.stack_super_graph_set:
                        if instr in self._supers_memo:
                                instr.nearest_supers = self._supers_memo[instr]
                                return instr.nearest_supers

                tmp_nearest_supers = set()
                self.stack_super_graph += [instr] #mark it as visited
                self.stack_super_graph_set.add(instr)
        #       print "Visiting %s prev %s" %(instr.name, prev == None and "Empty" or prev.name)
                if instr.is_super():
                        if prev != None and (prev, instr):
        #       print "Adding edge %s %s" %(prev.name, instr.name)
                                self.superedges |= set([(prev, instr)])
                                if ret_edge_in_path:
                                        self.super_ret_edges |= set([(prev, instr)])
                                        ret_edge_in_path = False
                        prev = instr
                        
                        self.supers += [instr]
                        instr.nearest_supers = set([instr])

                inctag_in_path |= instr.is_inctag()
                instr.inside_loop = inctag_in_path

                return_edge_pairs = getattr(self, "return_edge_pairs", None)
                if return_edge_pairs is None:
                        self.return_edge_pairs = set([(u, v) for (u, _up, v, _vp) in self.return_edges])
                        return_edge_pairs = self.return_edge_pairs

                for dest in [di for (_sp, di, _dp) in self._succ_map.get(instr, [])]:
                        if (instr, dest) in return_edge_pairs:
                                """true if there is a return edge between instr and dest, notice that this will not work correctly in the (unlikely) cases where there is a normal edge AND a return edge between instr and dest"""
                                debug.print_debug("INFO", "%s is a return edge." %([instr.name, dest.name]))
                                is_ret_edge = True
                        else:
                                is_ret_edge = ret_edge_in_path

                        if dest not in self.stack_super_graph_set: 
                                tmp_nearest_supers |= self.traverse_supers(prev, dest, is_ret_edge, inctag_in_path)
        
                        else:
                                #dest still in the stack, we add this to pending edges to resolve the dependencies at the end
                                if prev != None or instr.is_super():
                                        self.pending_superedges |= set([(prev, dest)])
                                        if ret_edge_in_path:
                                                self.pending_ret_edges |= set([(prev, dest)])
                


                

                if len(instr.nearest_supers) == 0: #if instr is not a super, we backpropagate the nearest supers of the targets
                #       print "%s is not a super" %instr
                        instr.nearest_supers = tmp_nearest_supers
        
                self.stack_super_graph.pop()
                self.stack_super_graph_set.discard(instr)
                if (not ret_edge_in_path) and (not inctag_in_path) and (not instr.is_super()):
                        self._supers_memo[instr] = instr.nearest_supers
                return instr.nearest_supers

        def _build_succ_map(self, edges):
                succ = {}
                for (s, s_p, d, d_p) in edges:
                        if s not in succ:
                                succ[s] = []
                        succ[s].append((s_p, d, d_p))
                return succ

        def _build_pred_map(self, edges):
                pred = {}
                for (s, s_p, d, d_p) in edges:
                        key = (d, d_p)
                        if key not in pred:
                                pred[key] = []
                        pred[key].append((s, s_p, d, d_p))
                return pred
