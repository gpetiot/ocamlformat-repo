let cmds = [ List.cmd; Get.cmd; Install.cmd ]

let () = Cmdliner.Term.(exit @@ eval_choice Default.cmd cmds)
