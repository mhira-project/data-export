split_patient_ids <- function(patient_ids, batch_size) {
  split(patient_ids, ceiling(seq_along(patient_ids) / batch_size))
}
