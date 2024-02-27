#' Notes and drums from variables
#'
#' @description Map dataframe variables to musical notes
#'
#' @name sonify_variables
#'
#' @import miditoolsR
#' @import midicsvR
#' @import midiplayR
#'
#' @param data dataframe to be mapped to notes
#' @param mapping list of mappings of variables to musical properties
#' @param instrument instrument number to be used for note events
#' @param base_time for time = 1 in milliseconds
#' @param start_time start time for playing notes
#' @param base_pitch offset for pitches
#' @param channel channel for MIDI notes.
#' @param default_velocity default velocity if not given through mapping
#' @param drum drum number from drum bank
#' @param drum_bank drum bank to be used

#' @return midi_event dataframe
#'
#' @rdname sonify_variables
#' @export
note_events <- function(
    data = NULL, mapping=props(),
    instrument = 1,
    base_time = 1,
    start_time = 0, base_pitch = 0,
    channel = 1, default_velocity = 0.8) {
  #' @import dplyr
  #' @import utils
  #' @import rlang
  note_props <- c(
    "time",
    "reltime",
    "pitch",
    "volume",
    "duration"
  )
  name_vec <- names(mapping)
  if (("time" %in% name_vec) & ("rel_time" %in% name_vec)) {
    stop("time and rel_time cannot be used in one mapping.")
  }
  if (!("pitch" %in% name_vec)) {
    stop("pitch needs to be mapped to a variable.")
  }
  if (!("duration" %in% name_vec)) {
    stop("duration needs to be mapped to a variable.")
  }
  names(mapping) |>
    purrr::keep(\(x)x %in% note_props) |>
    map(
      function(x) {
        eval_tidy(mapping[x][[1]], data) |>
          as_tibble() |>
          set_names(x)
      }
    ) |>
    bind_cols() -> result
 if ((!("time" %in% names(result))) & ("rel_time" %in% names(result))) {
    result <- bind_cols(time = cumsum(result$rel_time), result)
  }
  if ((!("time" %in% names(result))) & (!("rel_time" %in% names(result)))) {
    result <- bind_cols(time = c(0, head(cumsum(result$duration), -1)), result)
  }
  if (!("velocity" %in% names(result))) {
    result <- bind_cols(velocity = default_velocity, result)
  }
  bind_rows(
    instrument(start_time, channel, instrument),
    result |>
      mutate(channel = channel) |>
      select(.data$time, channel, .data$pitch, .data$velocity, .data$duration) |>
      mutate(pitch = .data$pitch + base_pitch) |>
      mutate(time = .data$time + start_time) |>
      mutate(time = .data$time * base_time) |>
      mutate(duration = .data$duration * base_time) |>
      expand_notes()
  )
}

expand_notes <- function(notes_df) {
  #' @importFrom purrr map
  notes_df |>
    split(1:nrow(notes_df)) |>
    map(\(x) with(x, note(time, channel, pitch, velocity, duration))) |>
    bind_rows() |>
    arrange(.data$time)
}

#' @rdname sonify_variables
#' @export
drum_events <- function(data,mapping=props(),
                        drum = 35,
                        base_time = 1,
                        start_time = 0,
                        default_velocity = 0.8,
                        drum_bank = 0) {
  drum_props <- c(
    "time",
    "reltime",
    "drum",
    "velocity",
    "duration"
  )
  channel <- 10
  name_vec <- names(mapping)
  if (("time" %in% name_vec) & ("rel_time" %in% name_vec)) {
    stop("time and rel_time cannot be used in one mapping.")
  }
  if (!("duration" %in% name_vec)) {
    stop("duration needs to be mapped to a variable.")
  }
  names(mapping) |>
    purrr::keep(\(x)x %in% drum_props) |>
    map(
      function(x) {
        eval_tidy(mapping[x][[1]], data) |>
          as_tibble() |>
          set_names(x)
      }
    ) |>
    bind_cols() -> result
  if ((!("time" %in% names(result))) & ("rel_time" %in% names(result))) {
    result <- bind_cols(time = cumsum(result$rel_time), result)
  }
  if ((!("time" %in% names(result))) & (!("rel_time" %in% names(result)))) {
    result <- bind_cols(time = c(0, head(cumsum(result$duration), -1)), result)
  }
  if (!("velocity" %in% names(result))) {
    result <- bind_cols(velocity = default_velocity,
                        result)  }
  if (!("drum" %in% names(result))) {
    result <- bind_cols(pitch=drum,
                        result)
  } else {
      result <-
        result |>
        rename(pitch=drum)
    }

  result <- bind_cols(result,channel=channel)
  bind_rows(
    drum_bank_select(start_time, channel, drum_bank),
    result |>
      mutate(time = .data$time + start_time) |>
      mutate(time = .data$time * base_time) |>
      mutate(duration = .data$duration * base_time) |>
      select(.data$time, channel, .data$pitch, .data$velocity, .data$duration) |>
      expand_notes()
  )

}

