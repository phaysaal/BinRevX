module SM = Map.Make (String)

type t = {
  env : string SM.t;
  mem : string SM.t;
  path : string list;
  location : string;
}

let empty = { env = SM.empty; mem = SM.empty; path = []; location = "entry" }

let bind st k v = { st with env = SM.add k v st.env }

let store st k v = { st with mem = SM.add k v st.mem }

let add_path st c = { st with path = st.path @ [ c ] }

let at st location = { st with location }

let of_inputs inputs =
  List.fold_left
    (fun st v -> bind st v ("sym_" ^ v))
    empty
    inputs

let render_env env =
  SM.bindings env
  |> List.map (fun (k, v) -> k ^ "=" ^ v)
  |> String.concat ","

let render_mem mem =
  SM.bindings mem
  |> List.map (fun (k, v) -> k ^ "=" ^ v)
  |> String.concat ","

let render st =
  Printf.sprintf
    "state(loc=%s, path=[%s], env=[%s], mem=[%s])"
    st.location
    (String.concat ", " st.path)
    (render_env st.env)
    (render_mem st.mem)
