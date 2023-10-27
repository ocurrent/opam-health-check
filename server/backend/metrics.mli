val number_of_packages : Prometheus.Gauge.t
val running : Prometheus.Gauge.t
val seconds_until_next_run : Prometheus.Gauge.t
val statistics : Prometheus.Gauge.family
val jobs_ok : Prometheus.Counter.t
val jobs_error : Prometheus.Counter.t
val jobs_total : Prometheus.Gauge.t
