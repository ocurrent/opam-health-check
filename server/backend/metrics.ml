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

let jobs_ok =
  let help = "Successfully completed jobs in the current run" in
  Counter.v ~help ~namespace ~subsystem "jobs_ok"

let jobs_error =
  let help = "Failed jobs in the current run" in
  Counter.v ~help ~namespace ~subsystem "jobs_error"

let jobs_total =
  let help = "Total number of jobs in the current run" in
  Gauge.v ~help ~namespace ~subsystem "jobs_total"
