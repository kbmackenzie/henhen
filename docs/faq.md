## Frequently Asked Questions

**Q:** *"Does HenHen replace `chicken-install`?"*

**A:** No, HenHen does *not* replace `chicken-install`. Quite the opposite: HenHen *invokes* `chicken-install` in a special environment to install eggs!

---

**Q:** *"Some CHICKEN tools are installed under different names on my system! How do I tell HenHen what name to use when invoking them?"*

**A:** You can define **global aliases** for each tool. [See the global alias documentation.](./aliases.md)

---

**Q:** *"Why is HenHen written in Haskell and not in CHICKEN Scheme?"*

**A:** Purely the author's preference. I absolutely adore Haskell, and I prefer using CHICKEN Scheme for small scripting tasks.

---

**Q:** *"Why use YAML for the configuration file instead of just using Scheme?*

**A:** Again, preference. YAML is more convenient.
