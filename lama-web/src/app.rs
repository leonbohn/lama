use crate::error_template::{AppError, ErrorTemplate};
use leptos::*;
use leptos_meta::*;
use leptos_router::*;

#[component]
pub fn App(cx: Scope) -> impl IntoView {
    // Provides context that manages stylesheets, titles, meta tags, etc.
    provide_meta_context(cx);

    view! {
        cx,

        // injects a stylesheet into the document <head>
        // id=leptos means cargo-leptos will hot-reload this stylesheet
        <Stylesheet id="leptos" href="/pkg/start-axum.css"/>

        // sets the document title
        <Title text="Welcome to Leptos"/>

        // content for this welcome page
        <Router fallback=|cx| {
            let mut outside_errors = Errors::default();
            outside_errors.insert_with_default_key(AppError::NotFound);
            view! { cx,
                <ErrorTemplate outside_errors/>
            }
            .into_view(cx)
        }>
            <main>
                <Routes>
                    <Route path="" view=|cx| view! { cx, <HomePage/> }/>
                </Routes>
            </main>
        </Router>
    }
}

#[server(Sprout, "/api/")]
pub async fn sprout(
    alphabet: String,
    positive: String,
    negative: String,
) -> Result<String, ServerFnError> {
    log!("SPTOU");
    Ok(format!(
        "Leon ist toll!\n{alphabet}\n{positive}\n{negative}"
    ))
}

/// Renders the home page of your application.
#[component]
fn HomePage(cx: Scope) -> impl IntoView {
    let (result, set_result) = create_signal(cx, "result".to_string());
    async fn call_sprout(val: (String, String, String)) -> String {
        let res = sprout(val.0, val.1, val.2).await.ok().unwrap();
        log!("RES: {}", res);
        res
    }
    // Creates a reactive value to update the button
    let (alphabet, set_alphabet) = create_signal(cx, "Alphabet".to_string());
    let (positive, set_positive) = create_signal(cx, "positive".to_string());
    let (negative, set_negative) = create_signal(cx, "negative".to_string());

    let sprouted = create_local_resource(
        cx,
        move || (alphabet.get(), positive.get(), negative.get()),
        call_sprout,
    );

    view! { cx,
        <h1>"Welcome to Leptos!" (sprouted.get())</h1>
        <input type="text" placeholder="alphabet" on:input=move |ev| { set_alphabet(event_target_value(&ev)) } prop:value = alphabet />
        <input type="text" placeholder="positive" on:input=move |ev| { set_positive(event_target_value(&ev)) } prop:value = positive />
        <input type="text" placeholder="negative" on:input=move |ev| { set_negative(event_target_value(&ev)) } prop:value = negative />
        <span>{result}</span>
        <button on:click = move |_| match sprouted.read(cx) {
            None => set_result("None".to_string()),
            Some(res) => set_result(res),
        }> " ASDF "</button>
    }
}
