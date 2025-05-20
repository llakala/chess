pub type State {
  State(can_passant: Bool, can_castle: Bool)
}

pub fn initial() {
  State(True, True)
}

pub type StateReturn(a) {
  StateReturn(value: a, state: State)
}

pub fn to_tuple(state_returned: StateReturn(a)) {
  #(state_returned.value, state_returned.state)
}
