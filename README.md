# 🌊 genflow - AI Generation Toolkit for R

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle: Experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/hugorteixeira/genflow/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hugorteixeira/genflow/actions/workflows/R-CMD-check.yaml)

<!-- badges: start -->
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

## 💡 Powerful Examples

### Text Generation That Speaks Volumes

```r
library(genflow)

# Transform complex concepts into clear explanations
result <- gen_txt(
  context = "Explain quantum computing in simple terms",
  service = "openai",
  model = "gpt-4o-mini",
  temp = 0.7
)

# Instantly visualize your results
gen_view(result)
```

### Image Generation That Captures Imagination

```r
# Bring your ideas to visual life
image_result <- gen_img(
  prompt = "A futuristic cityscape at sunset, hyper-realistic, 4K",
  service = "hf",  # Hugging Face
  model = "black-forest-labs/FLUX.1-schnell",
  h = 1024,
  w = 1024
)

# View your generated masterpiece
gen_view(image_result)
```

### Supercharged Batch Processing

```r
# Define your agent configurations
creator_a <- list(
  Service = "openai",
  Model = "gpt-4o",
  Temp = 0.8,
  Type = "Chat"
)

creator_b <- list(
  Service = "openai",
  Model = "gpt-4o-mini",
  Temp = 0.2,
  Type = "Chat"
)

# Deploy them in your environment
assign("creator_a", creator_a, envir = .GlobalEnv)
assign("creator_b", creator_b, envir = .GlobalEnv)

# Launch multi-agent generation campaigns
results <- gen_batch(
  qty = 4,
  instructions = "Write a creative story about the future of AI in data science",
  agent_prefix = "creator",
  directory = "generated_content"
)

# View all results at once
gen_view(results)
```

### Intelligent Model Management

```r
# Stay current with the latest models
gen_update_models()

# Discover the perfect model for your task
gen_show_models(provider = "openai", type = "chat")
```

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
| `gen_batch()` | Execute batch generation tasks |
| `gen_view()` | Interactive result visualization |

## 📊 Workflow Integration

genflow is designed to seamlessly integrate into your existing R workflows:

```r
# Use genflow results in your data analysis
text_result <- gen_txt("Analyze the following dataset trends: {data_summary}", ...)

# Incorporate generated insights into reports
# Create visualizations from generated data
# Build AI-powered dashboards
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