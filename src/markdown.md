{#what-is-vapor}

# What is Vapor?

{#vapor-is-a-frontend}

## Vapor is the frontend framework of Tether.

**No npm, no yarn, no package.json, no webpack, no bundlers, just Building.**

{#quickstart}

## Quickstart

%curl -sSL https://raw.githubusercontent.com/tether-labs/metal/main/install.sh | bash

%metal create vapor my-app

%cd my-app && metal run web

**Visit** [localhost:5173](http://localhost:5173/)

{#vapor-is-simple}

### Vapor is simple by nature

- Granular Automatic UI updates

- No framework rules, just normal programming.

- Only write `Zig`.

- Powerful Styling `.layout(.center)`

- Simplified memory management

- Native performance

{#why-zig}

### Why Zig?

Zig compiles directly to WebAssembly, giving you:

- **Native performance** - No JavaScript overhead

- **Small bundle sizes** - _Hello World_ in only **28kb**, including router, hooks, reactivity, and more

- **Memory control** - Optional manual management when needed

- **Simple mental model** - No transpilation, just compilation

{#how-it-works}

### How it works

- **Server-Side Pre-rendering** - Vapor compiles your Zig components into static HTML at build time. This is sent to the browser for an instant, SEO-friendly first paint.

- **Client-Side Hydration** - The browser also receives your compact vapor.wasm binary. This WASM binary runs and "hydrates" the static HTML, seamlessly taking control of the page.

- **Native Performance Runtime** - From that point on, all UI updates, routing, and logic are handled directly by high-performance WebAssembly, not JavaScript, giving you a smooth, native-like feel in the browser.

**You write Zig, it compiles to WASM, it runs in the browser. That's it.**

{#memory-is-not-scary}

### Memory is not Scary

If you’re new to low-level programming, you might assume memory management is hard.

**In Vapor, it’s not.**

Vapor, automates the UI memory management for you. If need be, you can use a set of memory functions,
which automatically free when it is no longer needed.

- `frameArray(T)` - A array allocated on the frame, and will be freed when the frame is rendered. (Frames: are just a single render cycle, ie FPS).
  This is useful, for arrays inside the UI, yes Vapor lets you write Zig code directly in your UI.

```zig
var array = Vapor.frameArray(u32);
array.append(1);
array.append(2);
array.append(3);
```

- `routeArray(T)` - A array allocated for the lifetime of the route, and will be freed when the route is changed.
  This is useful for arrays, that are only used in a single route, and are not needed after the route is changed. For example, a array of items in an
  ecommerce main page, and when switching to a product page, the array is no longer needed.

```zig
var array = Vapor.routeArray(u32);
array.append(1);
array.append(2);
array.append(3);
```

- `Array(T)` - A array allocated, and never freed. Will exist for the lifetime of the application.
  This is useful for arrays that are needed for the lifetime of the application, such as a list of items in a shopping cart. These arrays are never freed.
  Unless you manually free them.

```zig
var array = Vapor.Array(u32);
array.append(1);
array.append(2);
array.append(3);
```

{#making-a-button}

## Making a button!

We will jump into depth with styling, in the next section. For now though, we will make a **button.**

** As you can see, we do not allocate or use any memory, just simple functions.**

```zig
// All normal Zig code
const Vapor = @import("vapor");

// Components
const Button = Vapor.Button;
const Text = Vapor.Text;

var counter: i32 = 0;
fn increment() void {
    counter += 1;
}

// Render
pub fn render() void {
    Button(.{ .on_press = increment })
    .border(.simple(.black)).children({
        Text("Increment").fontSize(18).end();
    });
    Text(counter).fontSize(18).end();
}
```

@counter

Every Component follows the builder pattern. We start by creating a `Button` component, and then we can
call any set of **styling** functions such as `.border()`.

We attach a `on_press` handler to the button, and pass the increment function to it.

Within the `increment` function, we increment the counter, this will automatically result in the text being updated.

There is no need to use _Signals_ _Hooks_ or _State Management_ in Vapor.

Vapor is fully reactive and fine-grained: only the elements whos state has changed will be updated.

**No more useMemo, no state definitions — just simple variables.**

## How it works

Vapor runs the entire render cycle, on every state change. Vapor generates a Virtual Tree (DOM),
and then reconciles the differences between the old and new tree.

This is done in a single pass, and is extremely fast, even with large trees. Vapor can rerender a total of 10,000 nodes takes only 12ms on a 2021 M1 MacBook Pro.
**At 80FPS.**

After reconciliation, Vapor spits out an array of nodes:

- An array of nodes that need to be removed

- An array of nodes that need to be added

- An array of nodes that need to be updated

These are then applied to the DOM granularly for minimal overhead.

We can access these via the following commands:

```zig
const dirty_nodes = Vapor.dirty_nodes;
const added_nodes = Vapor.removed_nodes;
const removed_nodes = Vapor.added_nodes;

for (dirty_nodes.items) |node| {
    // Do something with the dirty node
}
```
