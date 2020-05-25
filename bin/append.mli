include Git.APPEND with type t = Lwt_unix.file_descr
                    and type uid = Fpath.t
                    and type error = [ `Msg of string ]
                    and type +'a fiber = 'a Lwt.t
