import "./style.css";
import { createClient } from "@supabase/supabase-js";
import { Elm } from "./Main.elm";

const app = Elm.Main.init({
  node: document.getElementById("app"),
  flags: import.meta.env.VITE_API_KEY,
});

const supabase_url = import.meta.env.VITE_APP_SUPABASE_URL;
const supabase_key = import.meta.env.VITE_APP_SUPABASE_ANON_KEY;

const supabase = createClient(supabase_url, supabase_key);

app.ports.sign_in_with_google.subscribe(async () => {
  const { error } = await supabase.auth.signInWithOAuth({
    provider: "google",
  });
  console.log("error", error);
});

app.ports.sign_in_with_github.subscribe(async () => {
  const { error } = await supabase.auth.signInWithOAuth({
    provider: "github",
  });
  console.log("error", error);
});

app.ports.sign_out.subscribe(async () => {
  const { error } = await supabase.auth.signOut();
  console.log("error", error);
});

app.ports.create_a_new_telegram_channel.subscribe(async (data) => {
  const { error } = await supabase.from("channels").insert(data);
  console.log("error", error);
});

app.ports.load_telegram_channels.subscribe(async () => {
  let { data } = await supabase.from("channels").select("*");
  app.ports.get_telegram_channels.send(data);
});

supabase.auth.onAuthStateChange((event, _session) => {
  if (event === "SIGNED_OUT") {
    app.ports.user_signed_out.send(null);
  } else {
    app.ports.user_signed_in.send(null);
  }
});
