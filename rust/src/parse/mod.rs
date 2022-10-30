mod internal;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Other {
        source: Box<dyn std::error::Error + 'static>,
    },

    #[error("unspecified parsing error")]
    Unspecified,
}

pub type Result<T> = std::result::Result<T, Error>;
