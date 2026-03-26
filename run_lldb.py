import lldb
debugger = lldb.SBDebugger.Create()
debugger.SetAsync(False)
target = debugger.CreateTarget("build/src/tokac")
bp = target.BreakpointCreateByLocation("Sema_Expr.cpp", 3225)
process = target.LaunchSimple(["tests/pass/closure_parse.tk"], None, None)
if process:
    thread = process.GetThreadAtIndex(0)
    frame = thread.GetSelectedFrame()
    print("LHS Expr type: ", frame.EvaluateExpression("Bin->LHS->toString()").GetSummary())
    print("lhsType->toString(): ", frame.EvaluateExpression("lhsType->toString()").GetSummary())
