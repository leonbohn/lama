fn main() {
    // Set up the tracing subscriber
    let subscriber = tracing_subscriber::fmt::Subscriber::builder().finish();

    tracing::subscriber::set_global_default(subscriber)
        .expect("Unable to set global tracing subscriber");
}
