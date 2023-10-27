open Prometheus

let namespace = "opam"
let subsystem = "health_check"

let number_of_packages =
  let help = "Total number packages in the run" in
  Gauge.v ~help ~namespace ~subsystem "number_of_packages"

let seconds_until_next_run =
  let help = "Number of seconds until the next automatic run" in
  Gauge.v ~help ~namespace ~subsystem "seconds_until_next_run"

let running =
  let help = "Run is in progress" in
  Gauge.v ~help ~namespace ~subsystem "running"

let statistics =
  let help = "Package status per switch" in
  Gauge.v_labels ~label_names:["switch"; "state"] ~help ~namespace ~subsystem "statistics"
