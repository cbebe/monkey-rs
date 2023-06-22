pub fn str_vec(a: &[impl ToString]) -> Vec<String> {
    a.iter().map(std::string::ToString::to_string).collect()
}
