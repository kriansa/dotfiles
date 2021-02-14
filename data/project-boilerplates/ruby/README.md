# Foobar

Foobar is a Ruby library for dealing with word pluralization.

## Installation

```bash
$ gem install foobar
```

## Usage

```ruby
require 'foobar'

Foobar.pluralize('word') # returns 'words'
Foobar.pluralize('goose') # returns 'geese'
Foobar.singularize('phenomena') # returns 'phenomenon'
```

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would
like to change.

Please make sure to update tests as appropriate. For more information, please refer to
[Contributing](CONTRIBUTING.md).

## License

This project is licensed under the Apache v2 License - see the [LICENSE](LICENSE) file for details.
