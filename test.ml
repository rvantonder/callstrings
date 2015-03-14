open Core_kernel.Std
open OUnit2

let f_callstring test_ctxt = 
  assert_command ~ctxt:test_ctxt "./callstrings.native" ["bin/example1"; "bin/example1.scm"; "10"; "--ocallstr"];
  In_channel.with_file "03.callstr" ~f:(fun chan -> 
    let st =
    In_channel.input_all chan in
    assert_equal st "(f((0x8458:32 0x841C:32)(0x8458:32 0x8434:32)(0x8458:32 0x8428:32)))\n")
  
let g_callstring test_ctxt = 
  assert_command ~ctxt:test_ctxt "./callstrings.native" ["bin/example1"; "bin/example1.scm"; "1"; "--ocallstr"];
  In_channel.with_file "03.callstr" ~f:(fun chan -> 
    let st =
    In_channel.input_all chan in
    assert_equal st "(g((0x841C:32 0x8434:32 0x8428:32)))\n")

let write_callsite_table test_ctxt = 
  assert_command ~ctxt:test_ctxt "./callstrings.native" ["bin/example1"; "bin/example1.scm"; "0"; "--tbl"];
  In_channel.with_file "03.tbl" ~f:(fun chan -> 
    let st =
    In_channel.input_all chan in
    assert_equal st "((((z 0x841C)(w 32)(signed false))g)(((z 0x8434)(w 32)(signed false))g)(((z 0x8458)(w 32)(signed false))f)(((z 0x8428)(w 32)(signed false))g))\n")

let load_callsite_table test_ctxt = 
  assert_command ~ctxt:test_ctxt "./callstrings.native" ["bin/example1"; "bin/example1.scm"; "10"; "--tbl"];
  assert_command ~ctxt:test_ctxt "./callstrings.native" ["bin/example1"; "bin/example1.scm"; "10"; "--tblfile"; "03.tbl"; "--ocallstr"];
  In_channel.with_file "03.callstr" ~f:(fun chan -> 
    let st =
    In_channel.input_all chan in
    assert_equal st "(f((0x8458:32 0x841C:32)(0x8458:32 0x8434:32)(0x8458:32 0x8428:32)))\n")

let write_dotfile test_ctxt = 
  assert_command ~ctxt:test_ctxt "./callstrings.native" ["bin/example1"; "bin/example1.scm"; "10"; "--dot"];
  In_channel.with_file "03.dot" ~f:(fun chan -> 
    let st =
    In_channel.input_all chan in
    assert_equal st "digraph G {\n  \"main\";\n  \"g\";\n  \"f\";\n  \n  \n  \"main\" -> \"g\" [label=\"0x8428:32\", ];\n  \"g\" -> \"f\" [label=\"0x8458:32\", ];\n  \"main\" -> \"g\" [label=\"0x8434:32\", ];\n  \"main\" -> \"g\" [label=\"0x841C:32\", ];\n  \n  }")

let suite =
  "suite">:::
    ["test f callstring">:: f_callstring;
     "test g callstring">:: g_callstring;
     "test write callsite table">:: write_callsite_table;
     "test load callsite table">:: load_callsite_table;
     "test write dotfile" >:: write_dotfile]

let () =
  run_test_tt_main suite
