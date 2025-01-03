## Frequently Asked Questions

**Q:** *"Does HenHen replace `chicken-install`?"*

**A:** No, HenHen does *not* replace `chicken-install`. Quite the opposite: HenHen *invokes* `chicken-install` in a special environment to install eggs!

---

**Q:** *"I can't build my egg with HenHen! What do I do?"*

**A:** Make sure the glob patterns in the `source-files` field also match `.egg` files!! HenHen needs to know where your egg file is in order to build your egg.

---

**Q:** *"Some CHICKEN tools are installed under different names on my system! How do I tell HenHen what name to use when invoking them?"*

**A:** You can define **global aliases** for each tool. [See the global alias documentation.](./aliases.md)

---

**Q:** *"Why is HenHen written in Haskell and not in CHICKEN Scheme?"*

**A:** Purely the author's preference. I absolutely adore Haskell, and I prefer using CHICKEN Scheme for small scripting tasks.

---

**Q:** *"Why use YAML for the configuration file instead of just using Scheme?"*

**A:** Again, preference. YAML is more convenient.

---

**Q:** *"I have encountered an issue/bug with HenHen! What do I do?"*

**A:** Feel free to open an issue on this repository and I will take a look at it as soon as I can! 🐔
