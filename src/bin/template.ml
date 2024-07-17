let template title body =
  let open Htmlit in
  let more_head =
    El.splice
      [
        El.link
          ~at:
            [
              At.rel "stylesheet";
              At.href
                "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/base-min.css";
            ]
          ();
        El.link
          ~at:
            [
              At.rel "stylesheet";
              At.href
                "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/grids-min.css";
            ]
          ();
        El.link
          ~at:
            [
              At.rel "stylesheet";
              At.href
                "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/grids-responsive-min.css";
            ]
          ();
        El.link
          ~at:
            [
              At.rel "stylesheet";
              At.href
                "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/buttons-min.css";
            ]
          ();
        El.link
          ~at:
            [
              At.rel "stylesheet";
              At.href
                "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/tables-min.css";
            ]
          ();
        El.link
          ~at:
            [
              At.rel "stylesheet";
              At.href "https://unpkg.com/leaflet@1.9.4/dist/leaflet.css";
            ]
          ();
        El.script
          ~at:[ At.src "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js" ]
          [];
      ]
  in
  let body =
    El.div
      ~at:[ At.class' "pure-g" ]
      [
        El.div ~at:[ At.class' "pure-u-1-8" ] [];
        El.div ~at:[ At.class' "pure-u-3-4" ] [ body ];
        El.div ~at:[ At.class' "pure-u-1-8" ] [];
      ]
  in
  El.page ~lang:"en" ~more_head ~title body
