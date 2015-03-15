open Core_kernel.Std
open Bap.Std
open Option
open Format
open Bap_plugins.Std
open Sexplib.Std

open Cmdliner

module Callgraph = Graph.Graphviz.Dot(struct
    type t = string table * string Addr.Table.t

    module V = struct
      type t = string
    end
    module E = struct
      type t = addr * (string * string)
      let src t = fst (snd t)
      let dst t = snd (snd t)
      type label = addr
      let label t = fst t
    end

    let iter_vertex f (syms, calls) = Table.iter syms ~f

    let iter_edges_e f (syms, calls) = Addr.Table.iter calls
        ~f:(fun ~key ~data -> Option.iter (Table.find_addr syms key)
               ~f:(fun (_, sym) -> f (key, (sym, data))))

    let vertex_name v = sprintf "%S" v
    let vertex_attributes v = []
    let default_vertex_attributes (syms, calls) = []
    let get_subgraph v = None
    let default_edge_attributes (syms, calls) = []
    let edge_attributes e = [`Label (Addr.to_string (E.label e))]
    let graph_attributes (syms, calls) = []
  end)
include Callgraph

let write_graph outfile symtbl calltbl =
  let file = Out_channel.create outfile in
  Callgraph.output_graph file (symtbl, calltbl);
  Format.printf "Writing graph: %a\n" String.pp outfile;
  Out_channel.close file

type kstring = string list with sexp
type 'a elem = string * 'a list with sexp

let write_callstrings outfile callsite_table callstrings = 
  let file = Out_channel.create outfile in
  Format.printf "Writing callstrings: %a\n" String.pp outfile;
  Format.set_formatter_out_channel file;
  let fname =
    Addr.Table.find_exn callsite_table 
      (Addr.of_string @@ List.hd_exn (List.hd_exn callstrings)) in
  Format.printf "%a\n" Sexp.pp @@
  (sexp_of_elem sexp_of_kstring) (fname, callstrings);
  Out_channel.close file;
  Format.set_formatter_out_channel stdout

let write_callsite_table outfile callsite_table = 
  let file = Out_channel.create outfile in
  Format.printf "Writing callsite table: %a\n" String.pp outfile;
  Format.set_formatter_out_channel file;
  Format.printf "%a\n" Sexp.pp @@
  (Addr.Table.sexp_of_t sexp_of_string callsite_table);
  Out_channel.close file;
  Format.set_formatter_out_channel stdout

(* This loads callstrings from our serialized file *)
let load_table tblfile =
  if String.is_empty tblfile then
    Addr.Table.create () else
    let sexp = Sexp.load_sexp tblfile in
    Addr.Table.t_of_sexp string_of_sexp sexp

(* This creates an Image from a file path *)
let create_image infile =
  match Image.create infile with
  | Ok (img,warns) ->
    (List.iter warns ~f:(fun x -> Format.eprintf "Warning: %a\n" Error.pp x);
     Format.printf "%-20s: %a\n" "Arch" Arch.pp (Image.arch img);
     Format.printf "%-20s: %a\n" "Entry" Addr.pp (Image.entry_point img);
     Format.printf "%-20s: %d\n" "Symbols" (Table.length (Image.symbols img));
     Format.printf "%-20s: %d\n" "Sections" (Table.length (Image.sections img));
     Some img)
  | Error err -> Format.eprintf "%a\n" Error.pp err; None

(* This loads symbols from our symbol file, exported from IDA *)
let load_symbols symfile arch mem =
  let sym_of_sexp x = <:of_sexp<string * int64 * int64>> x in
  In_channel.with_file symfile ~f:(fun out ->
      let buf = Lexing.from_channel out in
      Sexp.scan_fold_sexps buf ~init:Table.empty ~f:(fun syms_table sexp ->
          try
            let (name,es,ef) = sym_of_sexp sexp in
            let words = Int64.(ef - es |> to_int_exn) in
            let width = Arch.addr_size arch |> Size.to_bits in
            let from = Addr.of_int64 ~width es in
            let mem = Memory.view ~from ~words mem |> ok_exn in
            Format.printf "Loaded symbol: %a (%a)\n" 
              String.pp name Addr.pp (Memory.min_addr mem);
            Table.add syms_table mem name |> ok_exn
          with _ -> syms_table))

(* Recursive function for generating list of call strings (contexts) *)
let rec make_callstrings_rec callsite_table syms_table root k context_list =
  match k with
  | 0 -> context_list
  | _ when String.length root > 0 ->
    let inner = 
      callsite_table |> Addr.Table.fold ~init:context_list 
        ~f:(fun ~key ~data context_list ->
            match Table.find_addr syms_table key with
            | Some (_, sym) ->
              (* Prepend to inner list if we have a match *)
              if String.(sym = root) then 
                begin
                  Format.printf "%a: (%a) -> %a\n" 
                    String.pp sym Addr.pp key String.pp data;
                  let inner = (Addr.to_string key) :: (List.hd_exn context_list) in
                  make_callstrings_rec callsite_table syms_table data (k - 1)
                    (inner :: (List.tl_exn context_list)) 
                end
                (* Otherwise, don't change any list *)
              else context_list
            | None -> 
              Format.eprintf "Unresolved callsite: %a\n" Addr.pp key; context_list) in
    (* After the recursive step, prepend an empty inner list for the next iteration if necessary *)
    if List.length (Option.value (List.hd inner) ~default:[]) > 0 then 
      [] :: inner else inner
  | _ -> print_endline "Forest!\n"; context_list

(* Wrapper around recursive function to remove extra inner empty list *)
let make_callstrings callsite_table syms_table root k context_list =
  let result = make_callstrings_rec callsite_table syms_table root k context_list in
  if List.length result > 0 then 
    if List.length (List.hd_exn result) = 0 
    then List.tl_exn result else result
  else result

(* This function will do the iteration grunt work *)
let analyze_memory img mem sec symfile tblfile k out_dot out_callsite_tbl out_callstring =
  let arch = Image.arch img in
  (* syms_table stores a mapping of addresses to function names *)
  let syms_table = load_symbols symfile arch mem in
  let roots = Seq.(Table.regions syms_table >>| Memory.min_addr |> to_list) in
  let disasm = disassemble ~roots arch mem in
  let cfg = (Disasm.blocks disasm) in
  let blocks = Table.elements cfg in
  (* [callsite_table] stores a mapping of address of callsite to function name *)
  let callsite_table =  load_table tblfile in
  if Addr.Table.is_empty callsite_table then
    blocks |> Seq.iter ~f:(fun block -> 
        Format.printf "Parsing basic block: %a\n" Block.pp block;
        (Block.insns block) |> Seq.iter ~f:(fun (mem, insn) ->
            (Insn.bil insn) |> Bil.iter (object inherit [unit] Bil.visitor
              method! enter_int addr () =
                if in_jmp then
                  match Table.find_addr syms_table addr with
                  | Some (_, sym) ->
                    let addr = Memory.min_addr mem in
                    Format.printf "%a: %a -> %a\n" 
                      Addr.pp addr Insn.pp insn String.pp sym;
                    Addr.Table.add_exn callsite_table addr sym
                  | None -> ()
            end)));
  let result = 
    make_callstrings callsite_table syms_table "main" k [[]] in
  if out_dot then 
    write_graph (Section.name sec ^ ".dot") syms_table callsite_table;
  if out_callstring then 
    write_callstrings (Section.name sec ^ ".callstr") callsite_table result;
  if out_callsite_tbl then 
    write_callsite_table (Section.name sec ^ ".tbl") callsite_table

(* Given an image, iterate over each section and analyze it *)
let analyze_image infile symfile tblfile k out_dot out_tbl out_callstrings =
  create_image infile >>= fun img ->
  Table.iteri (Image.sections img) ~f:(fun mem s -> 
      if Section.is_executable s then
        analyze_memory img mem s symfile tblfile k out_dot out_tbl out_callstrings); 
  return ()


let infile =
  let doc = "Input binary file." in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~doc ~docv:"FILE")

let symfile =
  let doc = "Input symbol file." in
  Arg.(required & pos 1 (some non_dir_file) None & info [] ~doc ~docv:"SYMFILE")

let k =
  let doc = "Context sensitivity depth." in
  Arg.(required & pos 2 (some int) None & info [] ~doc ~docv:"K")

let dot =
  let doc = "Create graph output." in
  Arg.(value & flag & info ["dot"] ~doc ~docv:"DOT")

let tbl =
  let doc = "Create table output." in
  Arg.(value & flag & info ["tbl"] ~doc ~docv:"TBL")

let tblfile =
  let doc = "Input table file." in
  Arg.(value & opt (string) "" & info ["tblfile"] ~doc ~docv:"TBLFILE")

let out_callstrings =
  let doc = "Output callstrings" in
  Arg.(value & flag & info ["ocallstr"] ~doc ~docv:"CALLSTRTABLE")

let program =
  let doc = "Call Strings" in
  let man = [
    `S "DESCRIPTION";
    `P ("OVERVIEW: Generates call strings.") ] in
  Term.(pure analyze_image $infile $symfile $tblfile $k $dot $tbl $out_callstrings),
  Term.info "callstrings" ~doc ~man ~version:"1.0"

let () =
  Plugins.load ();
  let err = Format.err_formatter in
  at_exit (Format.pp_print_flush err);
  match Term.eval program ~err with
  | `Ok _ -> exit 0
  | `Error _ -> eprintf "Error parsing arguments\n"; exit 1
  | _ -> exit 2
