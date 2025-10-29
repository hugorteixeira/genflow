# 🌊 genflow - AI Generation Toolkit for R

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle: Experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R](https://img.shields.io/badge/R-%E2%89%A54.5-blue)](https://www.r-project.org/)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

> **Flow into the Future of AI**: genflow transforms your R workflows with seamless integration to the world's most powerful AI models. Generate text, images, and multimedia with unparalleled ease.

Dive into the next generation of AI-powered R programming with **genflow** — an intuitive, powerful toolkit that connects R with leading AI providers including OpenAI, OpenRouter, Hugging Face, Replicate, and FAL.

## ✨ Why genflow?

- 🚀 **Blazing Fast Integration**: Connect to multiple AI providers in seconds, not hours
- 🎯 **Intentional Design**: Built specifically for R workflows and data science pipelines
- 🌐 **Universal Provider Support**: OpenAI, OpenRouter, Hugging Face, Replicate, FAL, and more
- 📝 **Multi-Modal Mastery**: Text generation, image creation, audio processing, and beyond
- ⚡ **Optimized Performance**: Batch processing and parallel execution for maximum throughput
- 📊 **Smart Analytics**: Built-in tracking, logging, and performance metrics
- 🔄 **Seamless Updates**: Automatic model discovery and management
- 👁️ **Integrated Viewer**: Built-in visualization for all generated content
- 🧠 **Reusable Agents**: Define setups, content, and agents once, then reuse them across sessions with a single pipe

## 🚀 Getting Started

### Installation

```r
# Install the development version from GitHub
# install.packages("devtools")
devtools::install_github("hugorteixeira/genflow")
```

### Setup API Keys

Configure your AI provider credentials in your `.Renviron` file:

```r
# Add to your .Renviron file
OPENAI_API_KEY=your_openai_api_key_here
OPENROUTER_API_KEY=your_openrouter_api_key_here
HUGGINGFACE_API_TOKEN=your_huggingface_token_here
REPLICATE_API_TOKEN=your_replicate_token_here
FAL_API_KEY=your_fal_api_key_here
```

You can edit your `.Renviron` file by running:

```r
usethis::edit_r_environ()
```

After adding your keys, restart your R session for the changes to take effect.

## 💡 Examples

### Text Generation

```r
library(genflow)

# Transform complex concepts into clear explanations
result <- gen_txt(
  context = "Explain quantum computing like I'm dumb as a potato",
  service = "openai",
  model = "gpt-5",
  reasoning = "high"
)

# Instantly visualize your results
gen_view(result)
```

### Reusable Agents & Content (New!)

```r
# Capture a reusable setup
set_setup(
  sname   = "writer_openai",
  service = "openai",
  model   = "gpt-4o-mini",
  temp    = 0.7,
  type    = "Chat"
)

# Store supporting content
set_content(
  cname   = "launch_brief",
  context = "We are launching an AI-first analytics tool.",
  add     = "Audience: crazy data people who are in a serious relationship with R.",
  label   = "launch_announcement"
)

# Combine into an agent (automatically cached on disk)
creative_agent <- set_agent(
  name    = "creative_writer",
  setup   = "writer_openai",
  content = "launch_brief"
)

# Pipe agents directly into generators
creative_agent |> gen_txt()
creative_agent |> gen_img(prompt = "Richard Feynman and Michael Jackson playing Final Fantasy IX")

# Inspect everything that is cached
gen_list()
```

Agents, setups, and content are stored under `options("genflow.cache_dir")`
(defaults to `tools::R_user_dir("genflow", "cache")`), so they survive across R
sessions. A simple `get_agent("creative_writer")` in a future session gives you
a ready-to-use list that pipes straight into any generator.

Need an interface? Launch the interactive agent manager anytime with:

```r
gen_interface()
```

You’ll also find a “Launch Genflow Agent Interface” entry under the RStudio Addins menu for one-click access.

### Image Generation

```r
# Bring your ideas to visual life
image_result <- gen_img(
  prompt = "An image of existence if existence didn't exist and even thinking about existence would make it exist.",
  service = "hf",  # Hugging Face
  model = "black-forest-labs/FLUX.1-schnell",
  h = 1024,
  y = 1024
)

# View your generated masterpiece
gen_view(image_result)
```

### Supercharged Batch Processing

```r
# Load or build an agent
agent <- get_agent("creative_writer")

# Optional per-item data
one_item_each <- list(
  list(topic = "Healthcare analytics", tone = "optimistic"),
  list(topic = "Retail analytics", tone = "boring"),
  list(topic = "Financial forecasting", tone = "cynical")
)

# Run a batch in parallel – temporary agents are created and cleaned up automatically
results <- agent |> gen_batch_agent(
  qty = 3,
  instructions = "Write a 120-word launch announcement of a atomic bomb made of gummy bears.",
  one_item_each = one_item_each,
  directory = "generated_content"
)

gen_view(results)
```

### Intelligent Model Management

```r
# Stay current with the latest models
gen_update_models()

# Discover the perfect model for your task
gen_show_models(provider = "openai", type = "chat")
```


## 🧭 Example Workflow (Mermaid)

```mermaid
graph TD
  A[Define Setup] --> B[Create Content]
  B --> C[set_agent]
  C -->|Pipe| D[gen_txt]
  C -->|Pipe| E[gen_img]
  C -->|Pipe| F[gen_batch_agent]
  D --> G[gen_view]
  E --> G
  F --> G
  G --> H[Publish Report / Dashboard]
```

> _GitHub renders this Mermaid diagram automatically once committed. For local previews without Mermaid support, paste the snippet into [mermaid.live](https://mermaid.live/)._

## 🔧 Complete Function Reference

| Function | Purpose |
|---------|-------------|
| `gen_txt()` | Generate human-quality text with multimodal support |
| `gen_img()` | Create stunning images from text prompts |
| `gen_batch()` | Execute parallel generation campaigns |
| `gen_view()` | Visualize and explore generation results |
| `gen_stats()` | Analyze performance and usage metrics |
| `gen_stats_rm()` | Clean and manage statistics data |
| `gen_update_models()` | Refresh available models from all providers |
| `gen_show_models()` | Browse and filter available models |
| `set_setup()` / `get_setup()` / `list_setups()` | Persist provider configurations for reuse |
| `set_content()` / `get_content()` / `list_content()` | Store briefs, context, and other payloads |
| `set_agent()` / `get_agent()` / `list_agents()` | Combine setups and content into reusable agents |
| `mv_*()` / `rm_*()` | Rename or delete cached setups, content, or agents |
| `gen_list()` | Summarize everything saved in the cache directory |
| `gen_batch_agent()` | Run batch workloads directly from a `genflow_agent` |
| `gen_interface()` | Launch the interactive agent management interface (also available as an RStudio addin) |

## 📊 Workflow Integration

genflow is designed to seamlessly integrate into your existing R workflows:

```r
# Pull an agent and pass it straight into generators & pipelines
agent <- get_agent("creative_writer")

# Generate both narrative and visuals for your reports
copy <- agent |> gen_txt()
visual <- agent |> gen_img(prompt = "Millions of rubber ducks blitzkrieging Geneva")

# Feed generated assets into downstream analysis, dashboards, and reports
```

## 🛡️ Best Practices

### Production Readiness

- ⚡ **Monitor Usage**: Track API consumption with built-in statistics
- 🔐 **Secure Keys**: Keep API keys in environment variables, never in code
- 🧪 **Test Thoroughly**: Validate outputs for your specific use cases
- 📈 **Scale Smart**: Use batch processing for high-volume tasks

### Performance Tips

- 🔁 **Reuse Connections**: Initialize once, generate many
- 📦 **Manage Memory**: Use `gen_stats_rm()` to clean up old data
- ⚡ **Choose Wisely**: Select the right model for your specific task
- 🔄 **Update Regularly**: Run `gen_update_models()` periodically

## 🤝 Contributing

We welcome contributions! Whether it's:
- 🐛 Bug reports and fixes
- ✨ New feature proposals
- 📝 Documentation improvements
- 🎯 Performance optimizations

Check out our [Contributing Guidelines](CONTRIBUTING.md) to get started.

## 📄 License

This project is licensed under the GPL-3 License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

Special thanks to the AI provider communities and the R ecosystem for making genflow possible.

## 👨‍💻 About the Author

Hi, I'm Hugo. I build tools around trading, backtesting, and generative models in R to iterate on strategies faster and create amazing things. If you find genflow useful (or want to suggest improvements!), feedback is always welcome.

---

Project Link: [https://github.com/hugorteixeira/genflow](https://github.com/hugorteixeira/genflow)

---

<p align="center">Flow into the future of AI with ❤️ and ☕ in R</p>
